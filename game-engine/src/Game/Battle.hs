module Game.Battle where

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
--   - Determina orden de ataque basado en Speed stat
--   - Sin cambios de pokémon (TODO)
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
  let inTurn = bState {phase = TurnExecution}
      -- Determinar quién ataca primero basado en velocidad
      playerSpeed = speed (pStats (bpOriginal (playerActive inTurn)))
      enemySpeed = speed (pStats (bpOriginal (enemyActive inTurn)))
      playerFirst = playerSpeed > enemySpeed

      -- Aplicar acciones en orden
      (afterFirst, logFirst, rngAfterFirst) =
        if playerFirst
          then applyPlayerAction rng inTurn playerAction
          else applyEnemyAction rng inTurn enemyAction

      -- Chequear si el segundo atacante está debilitado
      defenderFainted = (if playerFirst then bpHp (enemyActive afterFirst) else bpHp (playerActive afterFirst)) <= 0

      -- Si el defensor no está debilitado, aplicar segundo ataque
      (afterSecond, logSecond, rngAfterSecond)
        | defenderFainted = (afterFirst, [], rngAfterFirst)
        | playerFirst = applyEnemyAction rngAfterFirst afterFirst enemyAction
        | otherwise = applyPlayerAction rngAfterFirst afterFirst playerAction

      finalState =
        afterSecond
          { phase = WaitingForCommand,
            turnCount = turnCount inTurn + 1
          }
   in (finalState {battleLog = battleLog finalState ++ logFirst ++ logSecond}, rngAfterSecond)

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
                        updatedEnemyPoke = enemyPoke {bpHp = newEnemyHp}
                        logMsg = pName (bpOriginal playerPoke) ++ " uses " ++ mName selectedMove ++ "! It does " ++ show damageDealt ++ " damage!"
                        -- Chequear si el enemigo quedó debilitado
                        isFainted = newEnemyHp <= 0
                        faintLog = [pName (bpOriginal enemyPoke) ++ " fainted!" | isFainted]
                        newState = bState {enemyActive = updatedEnemyPoke}
                     in (newState, logMsg : faintLog, rngAfterHitCheck)
  ActionSwitch _ ->
    -- TODO: Implementar cambios de pokemon
    (bState, ["Player switch (not yet implemented)"], rng)

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
                        updatedPlayerPoke = playerPoke {bpHp = newPlayerHp}
                        logMsg = pName (bpOriginal enemyPoke) ++ " uses " ++ mName selectedMove ++ "! It does " ++ show damageDealt ++ " damage!"
                        -- Chequear si el jugador quedó debilitado
                        isFainted = newPlayerHp <= 0
                        faintLog = ([pName (bpOriginal playerPoke) ++ " fainted!" | isFainted])
                        newState = bState {playerActive = updatedPlayerPoke}
                     in (newState, logMsg : faintLog, rngAfterHitCheck)
  ActionSwitch _ ->
    -- TODO: Implementar cambios de pokemon
    (bState, ["Enemy switch (not yet implemented)"], rng)
