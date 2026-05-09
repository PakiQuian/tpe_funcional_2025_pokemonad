module Pokemonad.Battle.Logic
  ( isAvailableForSwitch,
    firstAliveIndex,
    switchActive,
    updatePokemonAfterDamage,
    resolveTurnAfterDamage,
  )
where

import Data.List (findIndex)
import Pokemonad.Battle.State
  ( BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
    Side (..),
    Winner (..),
    getActive,
    getBench,
    setActive,
    setBench,
  )
import Pokemonad.Core.Pokemon (Pokemon (..))
import Pokemonad.Core.Types (HP (..), Status (..))

isAvailableForSwitch :: BattlePokemon -> Bool
isAvailableForSwitch bp = battlePokemonStatus bp /= Fainted

firstAliveIndex :: [BattlePokemon] -> Maybe Int
firstAliveIndex = findIndex isAvailableForSwitch

switchActive :: Side -> Int -> BattleState -> Either String BattleState
switchActive side benchIdx bState =
  case pickBenchPokemon benchIdx (getBench side bState) of
    Nothing -> Left "Invalid switch target."
    Just (target, remaining) ->
      if not (isAvailableForSwitch target)
        then Left "That Pokemon cannot be switched in."
        else
          let withOutgoing = setBench side (remaining ++ [getActive side bState]) bState
           in Right (setActive side target withOutgoing)

updatePokemonAfterDamage :: BattlePokemon -> HP -> BattlePokemon
updatePokemonAfterDamage pokemon newHp =
  let nextStatus = if unHP newHp <= 0 then Fainted else battlePokemonStatus pokemon
   in pokemon {battlePokemonHp = HP (max 0 (unHP newHp)), battlePokemonStatus = nextStatus}

resolveTurnAfterDamage :: BattleState -> (BattleState, [String])
resolveTurnAfterDamage bState
  | unHP (battlePokemonHp (enemyActive bState)) <= 0 =
      case firstAliveIndex (enemyBench bState) of
        Just idx ->
          case switchActive EnemySide idx bState of
            Right switched ->
              let switchedName = pokemonName (battlePokemonBase (enemyActive switched))
               in continuePlayerCheck switched ["Enemy sent out " ++ switchedName ++ "!"]
            Left _ -> continuePlayerCheck bState []
        Nothing ->
          (bState {phase = BattleEnded PlayerWon}, ["Enemy has no Pokemon left!"])
  | otherwise = continuePlayerCheck bState []

continuePlayerCheck :: BattleState -> [String] -> (BattleState, [String])
continuePlayerCheck bState logs
  | unHP (battlePokemonHp (playerActive bState)) <= 0 =
      case firstAliveIndex (playerBench bState) of
        Just _ -> (bState {phase = WaitingForForcedPlayerSwitch}, logs ++ ["Choose a replacement Pokemon."])
        Nothing -> (bState {phase = BattleEnded EnemyWon}, logs ++ ["You have no Pokemon left!"])
  | phase bState == BattleEnded PlayerWon = (bState, logs)
  | otherwise = (bState {phase = WaitingForCommand}, logs)

pickBenchPokemon :: Int -> [BattlePokemon] -> Maybe (BattlePokemon, [BattlePokemon])
pickBenchPokemon idx bench
  | idx < 0 || idx >= length bench = Nothing
  | otherwise =
      let (before, target : after) = splitAt idx bench
       in Just (target, before ++ after)
