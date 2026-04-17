module Game.Battle where

import Data.List (findIndex)
import Game.Logic
  ( canAttack,
    doesMoveHit,
    getAttackStat,
    getDefenseStat,
    resolveDamage,
  )
import Game.Move (Move (..), getMoveByName)
import Game.Pokemon (Pokemon (..), getPokemonById)
import Game.Trainer (AIDifficulty, Trainer (..))
import Game.Types (PokemonType (..), Stats (..), Status (..))
import System.Random (StdGen)

-- ==========================================
-- POKEMON DE BATALLA (Dinámico)
-- ==========================================
data BattlePokemon = BattlePokemon
  { bpOriginal :: Pokemon,
    bpHp :: Int,
    bpMaxHp :: Int,
    bpMoves :: [Move],
    bpStatus :: Status,
    bpLevel :: Int
  }
  deriving (Show, Eq)

-- ==========================================
-- ESTADO GLOBAL DE LA BATALLA
-- ==========================================
data BattlePhase
  = WaitingForCommand
  | TurnExecution
  | WaitingForForcedPlayerSwitch
  | BattleEnded Winner
  deriving (Show, Eq)

data Winner = PlayerWon | EnemyWon
  deriving (Show, Eq)

data BattleAction
  = ActionMove Int
  | ActionSwitch Int
  deriving (Show, Eq)

data BattleState = BattleState
  { playerActive :: BattlePokemon,
    playerBench :: [BattlePokemon],
    enemyActive :: BattlePokemon,
    enemyBench :: [BattlePokemon],
    enemyDifficulty :: AIDifficulty,
    turnCount :: Int,
    phase :: BattlePhase,
    battleLog :: [String]
  }
  deriving (Show, Eq)

-- ==========================================
-- INICIALIZACIÓN
-- ==========================================
initBattle :: [Int] -> Trainer -> BattleState
initBattle playerTeamIds enemyTrainer =
  let pTeam = map makeBattlePokemon playerTeamIds
      eTeam = map makeBattlePokemon (tTeamIds enemyTrainer)
   in BattleState
        { playerActive = head pTeam,
          playerBench = tail pTeam,
          enemyActive = head eTeam,
          enemyBench = tail eTeam,
          enemyDifficulty = tDifficulty enemyTrainer,
          turnCount = 1,
          phase = WaitingForCommand,
          battleLog = ["Battle started! Trainer " ++ tName enemyTrainer ++ " wants to battle!"]
        }

-- ===============================================================
-- Funciones Auxilares
-- ===============================================================
makeBattlePokemon :: Int -> BattlePokemon
makeBattlePokemon pid =
  case getPokemonById pid of
    Just p ->
      let lvl = 50
          mxHp = calculateHp (hp (pStats p)) lvl
       in BattlePokemon
            { bpOriginal = p,
              bpHp = mxHp,
              bpMaxHp = mxHp,
              bpMoves = pMoves p,
              bpStatus = Healthy,
              bpLevel = lvl
            }
    Nothing -> makeBattlePokemon 25

calculateHp :: Int -> Int -> Int
calculateHp baseStat level =
  (((baseStat + 31) * 2 * level) `div` 100) + level + 10

-- ===============================================================
-- RESOLUCIÓN DE TURNOS
-- ===============================================================
-- ALGORITMO ACTUAL:
-- Fase 1: Pre-resolución
--   - Si hay ActionSwitch (jugador o rival), el cambio tiene prioridad absoluta.
--   - Si hay switch + ataque, primero se realiza el switch y luego el ataque pega al nuevo activo.
--   - Si ambos hacen switch, se realizan ambos cambios y no hay ataques ese turno.
--   - Si no hay switches, el orden de ataques se determina por Speed.
-- Fase 2: Primer atacante ejecuta su movimiento
--   - Chequea estado alterado (Asleep, Frozen, etc.)
--   - Chequea precisión del movimiento
--   - Calcula daño usando Logic.resolveDamage
--   - Chequea si el defensor se debilitó (Faint)
-- Fase 3: Segundo atacante ejecuta si sigue vivo
--   - Mismo proceso que Fase 2
-- Fase 4: Fin de turno - Sin efectos residuales todavía
-- TODO: Switches, daño residual, efectos secundarios de movimientos

executeTurn :: StdGen -> BattleState -> BattleAction -> BattleAction -> (BattleState, StdGen)
executeTurn rng bState playerAction enemyAction =
  case phase bState of
    BattleEnded _ -> (bState, rng)
    WaitingForForcedPlayerSwitch ->
      case playerAction of
        ActionSwitch _ ->
          let (switchedState, switchLogs, nextRng) = applyPlayerAction rng bState playerAction
              keepForced = bpHp (playerActive switchedState) <= 0
              phaseAfter = if keepForced then WaitingForForcedPlayerSwitch else WaitingForCommand
              finalState = switchedState {phase = phaseAfter, battleLog = battleLog switchedState ++ switchLogs}
           in (finalState, nextRng)
        _ ->
          let forcedMsg = "You must switch Pokemon before continuing."
           in (bState {battleLog = battleLog bState ++ [forcedMsg]}, rng)
    _ ->
      let inTurn = bState {phase = TurnExecution}
          playerDidSwitch = case playerAction of
            ActionSwitch _ -> True
            _ -> False
          enemyDidSwitch = case enemyAction of
            ActionSwitch _ -> True
            _ -> False

          (afterSecond, logFirst, logSecond, rngAfterSecond)
            -- Ambos cambian: se resuelven ambos switches y termina el turno
            | playerDidSwitch && enemyDidSwitch =
                let (afterPlayerSwitch, playerSwitchLogs, rngAfterPlayerSwitch) = applyPlayerAction rng inTurn playerAction
                    (afterEnemySwitch, enemySwitchLogs, rngAfterEnemySwitch) = applyEnemyAction rngAfterPlayerSwitch afterPlayerSwitch enemyAction
                 in (afterEnemySwitch, playerSwitchLogs, enemySwitchLogs, rngAfterEnemySwitch)
            -- Jugador cambia primero, luego rival ataca al nuevo activo del jugador
            | playerDidSwitch =
                let (afterPlayerSwitch, playerSwitchLogs, rngAfterPlayerSwitch) = applyPlayerAction rng inTurn playerAction
                    (afterEnemyAttack, enemyAttackLogs, rngAfterEnemyAttack) =
                      if bpHp (enemyActive afterPlayerSwitch) <= 0
                        then (afterPlayerSwitch, [], rngAfterPlayerSwitch)
                        else applyEnemyAction rngAfterPlayerSwitch afterPlayerSwitch enemyAction
                 in (afterEnemyAttack, playerSwitchLogs, enemyAttackLogs, rngAfterEnemyAttack)
            -- Rival cambia primero, luego jugador ataca al nuevo activo del rival
            | enemyDidSwitch =
                let (afterEnemySwitch, enemySwitchLogs, rngAfterEnemySwitch) = applyEnemyAction rng inTurn enemyAction
                    (afterPlayerAttack, playerAttackLogs, rngAfterPlayerAttack) =
                      if bpHp (playerActive afterEnemySwitch) <= 0
                        then (afterEnemySwitch, [], rngAfterEnemySwitch)
                        else applyPlayerAction rngAfterEnemySwitch afterEnemySwitch playerAction
                 in (afterPlayerAttack, enemySwitchLogs, playerAttackLogs, rngAfterPlayerAttack)
            -- Si no hay switches, se mantiene lógica por velocidad
            | otherwise =
                let playerSpeed = speed (pStats (bpOriginal (playerActive inTurn)))
                    enemySpeed = speed (pStats (bpOriginal (enemyActive inTurn)))
                    playerFirst = playerSpeed > enemySpeed
                    (afterFirst, firstLogs, rngAfterFirst) =
                      if playerFirst
                        then applyPlayerAction rng inTurn playerAction
                        else applyEnemyAction rng inTurn enemyAction
                    defenderFainted = (if playerFirst then bpHp (enemyActive afterFirst) else bpHp (playerActive afterFirst)) <= 0
                    (afterSecondBySpeed, secondLogs, rngAfterSecondBySpeed)
                      | defenderFainted = (afterFirst, [], rngAfterFirst)
                      | playerFirst = applyEnemyAction rngAfterFirst afterFirst enemyAction
                      | otherwise = applyPlayerAction rngAfterFirst afterFirst playerAction
                 in (afterSecondBySpeed, firstLogs, secondLogs, rngAfterSecondBySpeed)

          (resolvedState, postLogs) = resolveTurnAfterDamage afterSecond
          finalState =
            resolvedState
              { turnCount = turnCount inTurn + 1,
                battleLog = battleLog resolvedState ++ logFirst ++ logSecond ++ postLogs
              }
       in (finalState, rngAfterSecond)

-- Aplica la acción del jugador (ataque)
applyPlayerAction :: StdGen -> BattleState -> BattleAction -> (BattleState, [String], StdGen)
applyPlayerAction rng bState action = case action of
  ActionMove moveIdx ->
    let playerPoke = playerActive bState
        enemyPoke = enemyActive bState
        playerMoves = bpMoves playerPoke
        -- Seleccionar movimiento
        selectedMove = if moveIdx < length playerMoves then playerMoves !! moveIdx else head playerMoves
        -- Chequear si el jugador puede atacar
        canPlayerAttack = canAttack (bpStatus playerPoke)
     in if not canPlayerAttack
          then
            let logMsg = pName (bpOriginal playerPoke) ++ " can't move!"
             in (bState, [logMsg], rng)
          else
            -- Chequear precisión
            let (hitsEnemy, rngAfterHitCheck) = doesMoveHit rng (mAccuracy selectedMove)
             in if not hitsEnemy
                  then
                    let logMsg = pName (bpOriginal playerPoke) ++ "'s " ++ mName selectedMove ++ " missed!"
                     in (bState, [logMsg], rngAfterHitCheck)
                  else
                    -- Calcular daño
                    let playerLevel = bpLevel playerPoke
                        enemyDefType = head (pType (bpOriginal enemyPoke))
                        atkStat = getAttackStat selectedMove (pStats (bpOriginal playerPoke))
                        defStat = getDefenseStat selectedMove (pStats (bpOriginal enemyPoke))
                        damageDealt = resolveDamage playerLevel atkStat defStat selectedMove enemyDefType
                        newEnemyHp = max 0 (bpHp enemyPoke - damageDealt)
                        updatedEnemyPoke = updatePokemonAfterDamage enemyPoke newEnemyHp
                        logMsg = pName (bpOriginal playerPoke) ++ " uses " ++ mName selectedMove ++ "! It does " ++ show damageDealt ++ " damage!"
                        -- Chequear si el enemigo quedó debilitado
                        isFainted = newEnemyHp <= 0
                        faintLog = [pName (bpOriginal enemyPoke) ++ " fainted!" | isFainted]
                        newState = bState {enemyActive = updatedEnemyPoke}
                     in (newState, logMsg : faintLog, rngAfterHitCheck)
  ActionSwitch benchIdx ->
    case switchPlayerActive benchIdx bState of
      Left errMsg -> (bState, [errMsg], rng)
      Right switchedState ->
        let switchedName = pName (bpOriginal (playerActive switchedState))
         in (switchedState, ["Player switched to " ++ switchedName ++ "!"], rng)

-- Aplica la acción del enemigo (ataque)
applyEnemyAction :: StdGen -> BattleState -> BattleAction -> (BattleState, [String], StdGen)
applyEnemyAction rng bState action = case action of
  ActionMove moveIdx ->
    let enemyPoke = enemyActive bState
        playerPoke = playerActive bState
        enemyMoves = bpMoves enemyPoke
        -- Seleccionar movimiento (índice proporcionado, por defecto 0 desde AI)
        selectedMove = if moveIdx < length enemyMoves then enemyMoves !! moveIdx else head enemyMoves
        -- Chequear si el enemigo puede atacar (estado alterado)
        canEnemyAttack = canAttack (bpStatus enemyPoke)
     in if not canEnemyAttack
          then
            let logMsg = pName (bpOriginal enemyPoke) ++ " can't move!"
             in (bState, [logMsg], rng)
          else
            -- Chequear precisión
            let (hitsPlayer, rngAfterHitCheck) = doesMoveHit rng (mAccuracy selectedMove)
             in if not hitsPlayer
                  then
                    let logMsg = pName (bpOriginal enemyPoke) ++ "'s " ++ mName selectedMove ++ " missed!"
                     in (bState, [logMsg], rngAfterHitCheck)
                  else
                    -- Calcular daño
                    let enemyLevel = bpLevel enemyPoke
                        atkStat = getAttackStat selectedMove (pStats (bpOriginal enemyPoke))
                        defStat = getDefenseStat selectedMove (pStats (bpOriginal playerPoke))
                        playerDefType = head (pType (bpOriginal playerPoke))
                        damageDealt = resolveDamage enemyLevel atkStat defStat selectedMove playerDefType
                        newPlayerHp = max 0 (bpHp playerPoke - damageDealt)
                        updatedPlayerPoke = updatePokemonAfterDamage playerPoke newPlayerHp
                        logMsg = pName (bpOriginal enemyPoke) ++ " uses " ++ mName selectedMove ++ "! It does " ++ show damageDealt ++ " damage!"
                        -- Chequear si el jugador quedó debilitado
                        isFainted = newPlayerHp <= 0
                        faintLog = ([pName (bpOriginal playerPoke) ++ " fainted!" | isFainted])
                        newState = bState {playerActive = updatedPlayerPoke}
                     in (newState, logMsg : faintLog, rngAfterHitCheck)
  ActionSwitch benchIdx ->
    case switchEnemyActive benchIdx bState of
      Left errMsg -> (bState, [errMsg], rng)
      Right switchedState ->
        let switchedName = pName (bpOriginal (enemyActive switchedState))
         in (switchedState, ["Enemy switched to " ++ switchedName ++ "!"], rng)

resolveTurnAfterDamage :: BattleState -> (BattleState, [String])
resolveTurnAfterDamage bState
  | bpHp (enemyActive bState) <= 0 =
      case firstAliveIndex (enemyBench bState) of
        Just idx ->
          case switchEnemyActive idx bState of
            Right switched ->
              let switchedName = pName (bpOriginal (enemyActive switched))
               in continuePlayerCheck switched ["Enemy sent out " ++ switchedName ++ "!"]
            Left _ -> continuePlayerCheck bState []
        Nothing ->
          let ended = bState {phase = BattleEnded PlayerWon}
           in (ended, ["Enemy has no Pokemon left!"])
  | otherwise = continuePlayerCheck bState []

continuePlayerCheck :: BattleState -> [String] -> (BattleState, [String])
continuePlayerCheck bState logs
  | bpHp (playerActive bState) <= 0 =
      case firstAliveIndex (playerBench bState) of
        Just _ -> (bState {phase = WaitingForForcedPlayerSwitch}, logs ++ ["Choose a replacement Pokemon."])
        Nothing -> (bState {phase = BattleEnded EnemyWon}, logs ++ ["You have no Pokemon left!"])
  | phase bState == BattleEnded PlayerWon = (bState, logs)
  | otherwise = (bState {phase = WaitingForCommand}, logs)

firstAliveIndex :: [BattlePokemon] -> Maybe Int
firstAliveIndex = findIndex isAvailableForSwitch

isAvailableForSwitch :: BattlePokemon -> Bool
isAvailableForSwitch bp = bpStatus bp /= Fainted

switchPlayerActive :: Int -> BattleState -> Either String BattleState
switchPlayerActive benchIdx bState =
  case pickBenchPokemon benchIdx (playerBench bState) of
    Nothing -> Left "Invalid player switch target."
    Just (target, remainingBench) ->
      if not (isAvailableForSwitch target)
        then Left "That Pokemon cannot be switched in."
        else
          let updatedBench = putOutgoingPokemonBackOnBench (playerActive bState) remainingBench
           in Right bState {playerActive = target, playerBench = updatedBench}

switchEnemyActive :: Int -> BattleState -> Either String BattleState
switchEnemyActive benchIdx bState =
  case pickBenchPokemon benchIdx (enemyBench bState) of
    Nothing -> Left "Invalid enemy switch target."
    Just (target, remainingBench) ->
      if not (isAvailableForSwitch target)
        then Left "Enemy cannot switch to that Pokemon."
        else
          let updatedBench = putOutgoingPokemonBackOnBench (enemyActive bState) remainingBench
           in Right bState {enemyActive = target, enemyBench = updatedBench}

putOutgoingPokemonBackOnBench :: BattlePokemon -> [BattlePokemon] -> [BattlePokemon]
putOutgoingPokemonBackOnBench outgoing remainingBench =
  remainingBench ++ [outgoing]

updatePokemonAfterDamage :: BattlePokemon -> Int -> BattlePokemon
updatePokemonAfterDamage pokemon newHp =
  let currentStatus = bpStatus pokemon
      nextStatus = if newHp <= 0 then Fainted else currentStatus
   in pokemon {bpHp = newHp, bpStatus = nextStatus}

pickBenchPokemon :: Int -> [BattlePokemon] -> Maybe (BattlePokemon, [BattlePokemon])
pickBenchPokemon idx bench
  | idx < 0 || idx >= length bench = Nothing
  | otherwise =
      let (before, target : after) = splitAt idx bench
       in Just (target, before ++ after)
