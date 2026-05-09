module Pokemonad.Battle.Turn
  ( executeTurn,
    applyAction,
    submitPlayerActionWithEnemyWeights,
  )
where

import Pokemonad.Battle.Damage (canAttack, doesMoveHit, getAttackStat, getDefenseStat, resolveDamage)
import Pokemonad.Battle.Logic (isAvailableForSwitch, resolveTurnAfterDamage, switchActive, updatePokemonAfterDamage)
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePokemon (..),
    BattlePhase (..),
    BattleState (..),
    Side (..),
    Winner (..),
    getActive,
    getBench,
    setActive,
  )
import Pokemonad.AI.Decision (QWeights, chooseEnemyActionWithMaybeWeights)
import Pokemonad.Core.Move (Move (..))
import Pokemonad.Core.Pokemon (Pokemon (..))
import Pokemonad.Core.Types (HP (..), Level (..), PokemonType (..), Stats (..))
import System.Random (StdGen, split)

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
           in if not (canAttack (battlePokemonStatus activePoke))
                then (bState, [pokemonName (battlePokemonBase activePoke) ++ " can't move!"], rng)
                else
                  let (hits, rngAfterHit) = doesMoveHit rng (moveAccuracy selectedMove)
                   in if not hits
                        then
                          ( bState,
                            [pokemonName (battlePokemonBase activePoke) ++ "'s " ++ moveName selectedMove ++ " missed!"],
                            rngAfterHit
                          )
                        else
                          let atkStat = getAttackStat selectedMove (pokemonStats (battlePokemonBase activePoke))
                              defStat = getDefenseStat selectedMove (pokemonStats (battlePokemonBase opponentPoke))
                              defType = head (pokemonTypes (battlePokemonBase opponentPoke))
                              damage = resolveDamage (battlePokemonLevel activePoke) atkStat defStat selectedMove defType
                              newOpponentHp = HP (unHP (battlePokemonHp opponentPoke) - unHP damage)
                              updatedOpponent = updatePokemonAfterDamage opponentPoke newOpponentHp
                              hitLog = pokemonName (battlePokemonBase activePoke) ++ " uses " ++ moveName selectedMove ++ "! It does " ++ show (unHP damage) ++ " damage!"
                              faintLog = [pokemonName (battlePokemonBase opponentPoke) ++ " fainted!" | unHP newOpponentHp <= 0]
                              newState = setActive opponentSide updatedOpponent bState
                           in (newState, hitLog : faintLog, rngAfterHit)
        ActionSwitch benchIdx ->
          case switchActive side benchIdx bState of
            Left errMsg -> (bState, [errMsg], rng)
            Right switched ->
              let switchedName = pokemonName (battlePokemonBase (getActive side switched))
               in (switched, [sideLabel side ++ " switched to " ++ switchedName ++ "!"], rng)

executeTurn :: StdGen -> BattleState -> BattleAction -> BattleAction -> (BattleState, StdGen)
executeTurn rng bState playerAction enemyAction =
  case phase bState of
    BattleEnded _ -> (bState, rng)
    WaitingForForcedPlayerSwitch ->
      case playerAction of
        ActionSwitch _ ->
          let (switched, switchLogs, rng1) = applyAction PlayerSide rng bState playerAction
              keepForced = unHP (battlePokemonHp (playerActive switched)) <= 0
              phaseAfter = if keepForced then WaitingForForcedPlayerSwitch else WaitingForCommand
           in (switched {phase = phaseAfter, battleLog = battleLog switched ++ switchLogs}, rng1)
        _ ->
          (bState {battleLog = battleLog bState ++ ["You must switch Pokemon before continuing."]}, rng)
    _ ->
      let playerDidSwitch = isSwitchAction playerAction
          enemyDidSwitch = isSwitchAction enemyAction
          (afterActions, logsFirst, logsSecond, rngFinal)
            | playerDidSwitch && enemyDidSwitch =
                let (s1, l1, r1) = applyAction PlayerSide rng bState playerAction
                    (s2, l2, r2) = applyAction EnemySide r1 s1 enemyAction
                 in (s2, l1, l2, r2)
            | playerDidSwitch =
                let (s1, l1, r1) = applyAction PlayerSide rng bState playerAction
                    (s2, l2, r2) = if unHP (battlePokemonHp (enemyActive s1)) <= 0
                                     then (s1, [], r1)
                                     else applyAction EnemySide r1 s1 enemyAction
                 in (s2, l1, l2, r2)
            | enemyDidSwitch =
                let (s1, l1, r1) = applyAction EnemySide rng bState enemyAction
                    (s2, l2, r2) = if unHP (battlePokemonHp (playerActive s1)) <= 0
                                     then (s1, [], r1)
                                     else applyAction PlayerSide r1 s1 playerAction
                 in (s2, l1, l2, r2)
            | otherwise =
                let playerFirst = statsSpeed (pokemonStats (battlePokemonBase (playerActive bState)))
                                > statsSpeed (pokemonStats (battlePokemonBase (enemyActive bState)))
                    (firstSide, firstAction, secondSide, secondAction) =
                      if playerFirst
                        then (PlayerSide, playerAction, EnemySide, enemyAction)
                        else (EnemySide, enemyAction, PlayerSide, playerAction)
                    (s1, l1, r1) = applyAction firstSide rng bState firstAction
                    defenderFainted = unHP (battlePokemonHp (getActive secondSide s1)) <= 0
                    (s2, l2, r2) = if defenderFainted
                                     then (s1, [], r1)
                                     else applyAction secondSide r1 s1 secondAction
                 in (s2, l1, l2, r2)
          (resolved, postLogs) = resolveTurnAfterDamage afterActions
          finalState = resolved
            { turnCount = turnCount bState + 1,
              battleLog = battleLog resolved ++ logsFirst ++ logsSecond ++ postLogs
            }
       in (finalState, rngFinal)

submitPlayerActionWithEnemyWeights :: Maybe QWeights -> StdGen -> BattleState -> BattleAction -> (BattleState, StdGen)
submitPlayerActionWithEnemyWeights maybeWeights rng bState playerAction =
  case phase bState of
    BattleEnded _ -> (bState, rng)
    WaitingForForcedPlayerSwitch ->
      case playerAction of
        ActionSwitch _ ->
          let (enemyAction, rng1) = chooseEnemyActionWithMaybeWeights rng (enemyDifficulty bState) maybeWeights bState
           in executeTurn rng1 bState playerAction enemyAction
        _ ->
          ( bState {battleLog = battleLog bState ++ ["You must switch Pokemon before selecting another action."]},
            rng
          )
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
