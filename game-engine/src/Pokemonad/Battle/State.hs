module Pokemonad.Battle.State
  ( BattlePokemon (..),
    BattlePhase (..),
    BattleAction (..),
    Winner (..),
    BattleState (..),
    Side (..),
    getActive,
    setActive,
    getBench,
    setBench,
    initBattle,
    makeBattlePokemon,
  )
where

import Pokemonad.Core.Move (Move)
import Pokemonad.Core.Pokemon (Pokemon (..), getPokemonById)
import Pokemonad.Core.Trainer (AIDifficulty, Trainer (..))
import Pokemonad.Core.Types (HP (..), Level (..), PokemonId (..), Stats (..), Status (..))

data BattlePokemon = BattlePokemon
  { battlePokemonBase :: Pokemon,
    battlePokemonHp :: HP,
    battlePokemonMaxHp :: HP,
    battlePokemonMoves :: [Move],
    battlePokemonStatus :: Status,
    battlePokemonLevel :: Level
  }
  deriving (Show, Eq)

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

data Side = PlayerSide | EnemySide
  deriving (Show, Eq)

-- Accessors for symmetric battle operations

getActive :: Side -> BattleState -> BattlePokemon
getActive PlayerSide = playerActive
getActive EnemySide = enemyActive

setActive :: Side -> BattlePokemon -> BattleState -> BattleState
setActive PlayerSide bp s = s {playerActive = bp}
setActive EnemySide bp s = s {enemyActive = bp}

getBench :: Side -> BattleState -> [BattlePokemon]
getBench PlayerSide = playerBench
getBench EnemySide = enemyBench

setBench :: Side -> [BattlePokemon] -> BattleState -> BattleState
setBench PlayerSide bench s = s {playerBench = bench}
setBench EnemySide bench s = s {enemyBench = bench}

initBattle :: [PokemonId] -> Trainer -> BattleState
initBattle playerTeamIds enemyTrainer =
  let pTeam = map makeBattlePokemon playerTeamIds
      eTeam = map makeBattlePokemon (trainerTeam enemyTrainer)
   in BattleState
        { playerActive = head pTeam,
          playerBench = tail pTeam,
          enemyActive = head eTeam,
          enemyBench = tail eTeam,
          enemyDifficulty = trainerDifficulty enemyTrainer,
          turnCount = 1,
          phase = WaitingForCommand,
          battleLog = ["Battle started! Trainer " ++ trainerName enemyTrainer ++ " wants to battle!"]
        }

makeBattlePokemon :: PokemonId -> BattlePokemon
makeBattlePokemon pid =
  case getPokemonById pid of
    Just p ->
      let lvl = Level 50
          maxHp = HP (calculateBaseHp (statsHp (pokemonStats p)) lvl)
       in BattlePokemon
            { battlePokemonBase = p,
              battlePokemonHp = maxHp,
              battlePokemonMaxHp = maxHp,
              battlePokemonMoves = pokemonMoves p,
              battlePokemonStatus = Healthy,
              battlePokemonLevel = lvl
            }
    Nothing -> makeBattlePokemon (PokemonId 25)

calculateBaseHp :: Int -> Level -> Int
calculateBaseHp baseStat (Level level) =
  (((baseStat + 31) * 2 * level) `div` 100) + level + 10
