module Pokemonad.Battle.Turn
  ( BattleStep,
    finalStateOf,
    executeTurn,
    executeTurnMulti,
    applyAction,
    submitPlayerActionWithEnemyWeights,
  )
where

import Pokemonad.AI.Decision (QWeights, chooseEnemyActionWithMaybeWeights)
import Pokemonad.Battle.Damage (canAttack, doesMoveHit, getAttackStat, getDefenseStat, resolveDamage)
import Pokemonad.Battle.Logic (resolveTurnAfterDamage, resolveTurnAfterDamageMulti, switchActive, updatePokemonAfterDamage)
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
    Side (..),
    enemyActive,
    getActive,
    playerActive,
    setActive,
  )
import Pokemonad.Core.Move (Move (..))
import Pokemonad.Core.Pokemon (Pokemon (..))
import Pokemonad.Core.Types (HP (..), Stats (..))
import System.Random (StdGen)

-- | One animated step of a battle: the state after that step, plus the log
--   lines that were added in this step. The state's `battleLog` is cumulative
--   up to and including this step; the [String] is only the *new* lines.
type BattleStep = (BattleState, [String])

-- | Resolver applied after both sides have acted. Different in single-player
--   (auto-switches enemy) vs multiplayer (waits for opponent to choose).
type TurnResolver = BattleState -> (BattleState, [String])

-- | Take the final BattleState from a frame sequence; fall back to the
--   provided state if there were no frames (e.g., the turn was a no-op).
finalStateOf :: BattleState -> ([BattleStep], StdGen) -> (BattleState, StdGen)
finalStateOf fallback (steps, rng) =
  case steps of
    [] -> (fallback, rng)
    _ -> (fst (last steps), rng)

-- | Apply one side's action to the battle state, returning the updated state,
--   a log of what happened, and the next RNG.
applyAction :: Side -> StdGen -> BattleState -> BattleAction -> (BattleState, [String], StdGen)
applyAction side rng bState action =
  let activePoke = getActive side bState
      opponentSide = oppositeSide side
      opponentPoke = getActive opponentSide bState
   in case action of
        ActionMove moveIdx ->
          let moves = battlePokemonMoves activePoke
              selectedMove = if moveIdx < length moves then moves !! moveIdx else head moves
              attackerName = pokemonName (battlePokemonBase activePoke)
              defenderName = pokemonName (battlePokemonBase opponentPoke)
              canMove = canAttack (battlePokemonStatus activePoke)
              (hits, rng') = doesMoveHit rng (moveAccuracy selectedMove)
              atkStat = getAttackStat selectedMove (pokemonStats (battlePokemonBase activePoke))
              defStat = getDefenseStat selectedMove (pokemonStats (battlePokemonBase opponentPoke))
              defType = head (pokemonTypes (battlePokemonBase opponentPoke))
              damage = resolveDamage (battlePokemonLevel activePoke) atkStat defStat selectedMove defType
              newOpponentHp = HP (unHP (battlePokemonHp opponentPoke) - unHP damage)
              updatedOpponent = updatePokemonAfterDamage opponentPoke newOpponentHp
              hitLog = attackerName ++ " uses " ++ moveName selectedMove ++ "! It does " ++ show (unHP damage) ++ " damage!"
              faintLog = [defenderName ++ " fainted!" | unHP newOpponentHp <= 0]
              newState = setActive opponentSide updatedOpponent bState
           in if not canMove
                then (bState, [attackerName ++ " can't move!"], rng)
                else
                  if not hits
                    then (bState, [attackerName ++ "'s " ++ moveName selectedMove ++ " missed!"], rng')
                    else (newState, hitLog : faintLog, rng')
        ActionSwitch benchIdx ->
          case switchActive side benchIdx bState of
            Left errMsg -> (bState, [errMsg], rng)
            Right switched ->
              let switchedName = pokemonName (battlePokemonBase (getActive side switched))
               in (switched, [sideLabel side ++ " switched to " ++ switchedName ++ "!"], rng)

-- | Single-player: produce the per-step frame sequence for a turn.
executeTurn :: StdGen -> BattleState -> BattleAction -> BattleAction -> ([BattleStep], StdGen)
executeTurn rng bState playerAction enemyAction =
  case phase bState of
    BattleEnded _ -> ([], rng)
    WaitingForForcedPlayerSwitch -> forcedPlayerSwitchFrames rng bState playerAction
    _ -> executeTurnFrames resolveTurnAfterDamage rng bState playerAction enemyAction

-- | Multiplayer: produce the per-step frame sequence for a turn. The host
--   computes frames locally and ships a flipped copy to the peer; both sides
--   animate independently.
executeTurnMulti :: StdGen -> BattleState -> BattleAction -> BattleAction -> ([BattleStep], StdGen)
executeTurnMulti rng bState playerAction enemyAction =
  case phase bState of
    BattleEnded _ -> ([], rng)
    WaitingForForcedPlayerSwitch -> forcedPlayerSwitchFrames rng bState playerAction
    WaitingForForcedEnemySwitch -> forcedEnemySwitchFrames rng bState enemyAction
    _ -> executeTurnFrames resolveTurnAfterDamageMulti rng bState playerAction enemyAction

-- | Core: run both actions, then the resolver, emitting one frame per visible
--   sub-step. Skips frames that produced no logs and no phase change.
executeTurnFrames ::
  TurnResolver ->
  StdGen ->
  BattleState ->
  BattleAction ->
  BattleAction ->
  ([BattleStep], StdGen)
executeTurnFrames resolver rng bState playerAction enemyAction =
  let baseLog = battleLog bState
      newTurnCount = turnCount bState + 1
      (s1, l1, s2, l2, rngFinal) = applyBothActionsTraced rng bState playerAction enemyAction
      (resolved, postLogs) = resolver s2

      step1 = mkFrame newTurnCount s1 (baseLog ++ l1) l1
      step2 = mkFrame newTurnCount s2 (baseLog ++ l1 ++ l2) l2
      step3 = mkFrame newTurnCount resolved (baseLog ++ l1 ++ l2 ++ postLogs) postLogs

      secondActionFired = not (null l2)
      phaseChanged = phase resolved /= phase s2
      keepStep3 = not (null postLogs) || phaseChanged

      frames =
        step1
          : ([step2 | secondActionFired])
          ++ ([step3 | keepStep3])
   in (frames, rngFinal)

mkFrame :: Int -> BattleState -> [String] -> [String] -> BattleStep
mkFrame newTurnCount state logSoFar newLogs =
  (state {turnCount = newTurnCount, battleLog = logSoFar}, newLogs)

-- | Apply player's and enemy's actions in the right order; expose both the
--   intermediate (post-first-action) and final (post-second-action) states.
--   When the second action is skipped, the second state equals the first.
applyBothActionsTraced ::
  StdGen ->
  BattleState ->
  BattleAction ->
  BattleAction ->
  (BattleState, [String], BattleState, [String], StdGen)
applyBothActionsTraced rng bState playerAction enemyAction
  | playerDidSwitch && enemyDidSwitch =
      let (s1, l1, r1) = applyAction PlayerSide rng bState playerAction
          (s2, l2, r2) = applyAction EnemySide r1 s1 enemyAction
       in (s1, l1, s2, l2, r2)
  | playerDidSwitch =
      let (s1, l1, r1) = applyAction PlayerSide rng bState playerAction
          enemyFainted = unHP (battlePokemonHp (enemyActive s1)) <= 0
          (s2, l2, r2) = if enemyFainted then (s1, [], r1) else applyAction EnemySide r1 s1 enemyAction
       in (s1, l1, s2, l2, r2)
  | enemyDidSwitch =
      let (s1, l1, r1) = applyAction EnemySide rng bState enemyAction
          playerFainted = unHP (battlePokemonHp (playerActive s1)) <= 0
          (s2, l2, r2) = if playerFainted then (s1, [], r1) else applyAction PlayerSide r1 s1 playerAction
       in (s1, l1, s2, l2, r2)
  | otherwise =
      let playerSpeed = statsSpeed (pokemonStats (battlePokemonBase (playerActive bState)))
          enemySpeed = statsSpeed (pokemonStats (battlePokemonBase (enemyActive bState)))
          playerFirst = playerSpeed > enemySpeed
          (firstSide, firstAction, secondSide, secondAction) =
            if playerFirst
              then (PlayerSide, playerAction, EnemySide, enemyAction)
              else (EnemySide, enemyAction, PlayerSide, playerAction)
          (s1, l1, r1) = applyAction firstSide rng bState firstAction
          defenderFainted = unHP (battlePokemonHp (getActive secondSide s1)) <= 0
          (s2, l2, r2) = if defenderFainted then (s1, [], r1) else applyAction secondSide r1 s1 secondAction
       in (s1, l1, s2, l2, r2)
  where
    playerDidSwitch = isSwitchAction playerAction
    enemyDidSwitch = isSwitchAction enemyAction

-- | One-frame sequence for the player's forced switch.
forcedPlayerSwitchFrames :: StdGen -> BattleState -> BattleAction -> ([BattleStep], StdGen)
forcedPlayerSwitchFrames rng bState playerAction =
  case playerAction of
    ActionSwitch _ ->
      let (switched, switchLogs, rng1) = applyAction PlayerSide rng bState playerAction
          stillFainted = unHP (battlePokemonHp (playerActive switched)) <= 0
          phaseAfter = if stillFainted then WaitingForForcedPlayerSwitch else WaitingForCommand
          frameState = switched {phase = phaseAfter, battleLog = battleLog switched ++ switchLogs}
       in ([(frameState, switchLogs)], rng1)
    _ ->
      let msg = "You must switch Pokemon before continuing."
          frameState = bState {battleLog = battleLog bState ++ [msg]}
       in ([(frameState, [msg])], rng)

-- | One-frame sequence for the enemy's forced switch (multiplayer only).
forcedEnemySwitchFrames :: StdGen -> BattleState -> BattleAction -> ([BattleStep], StdGen)
forcedEnemySwitchFrames rng bState enemyAction =
  case enemyAction of
    ActionSwitch _ ->
      let (switched, switchLogs, rng1) = applyAction EnemySide rng bState enemyAction
          stillFainted = unHP (battlePokemonHp (enemyActive switched)) <= 0
          phaseAfter = if stillFainted then WaitingForForcedEnemySwitch else WaitingForCommand
          frameState = switched {phase = phaseAfter, battleLog = battleLog switched ++ switchLogs}
       in ([(frameState, switchLogs)], rng1)
    _ ->
      let msg = "Waiting for opponent to switch."
          frameState = bState {battleLog = battleLog bState ++ [msg]}
       in ([(frameState, [msg])], rng)

submitPlayerActionWithEnemyWeights :: Maybe QWeights -> StdGen -> BattleState -> BattleAction -> ([BattleStep], StdGen)
submitPlayerActionWithEnemyWeights maybeWeights rng bState playerAction =
  case phase bState of
    BattleEnded _ -> ([], rng)
    WaitingForForcedPlayerSwitch ->
      case playerAction of
        ActionSwitch _ ->
          let (enemyAction, rng1) = chooseEnemyActionWithMaybeWeights rng (enemyDifficulty bState) maybeWeights bState
           in executeTurn rng1 bState playerAction enemyAction
        _ ->
          let msg = "You must switch Pokemon before selecting another action."
              frameState = bState {battleLog = battleLog bState ++ [msg]}
           in ([(frameState, [msg])], rng)
    _ ->
      let (enemyAction, rng1) = chooseEnemyActionWithMaybeWeights rng (enemyDifficulty bState) maybeWeights bState
       in executeTurn rng1 bState playerAction enemyAction

-- Helpers

oppositeSide :: Side -> Side
oppositeSide PlayerSide = EnemySide
oppositeSide EnemySide = PlayerSide

isSwitchAction :: BattleAction -> Bool
isSwitchAction (ActionSwitch _) = True
isSwitchAction _ = False

sideLabel :: Side -> String
sideLabel PlayerSide = "Player"
sideLabel EnemySide = "Enemy"
