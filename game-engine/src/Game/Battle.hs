module Game.Battle where

import Game.Move (Move (..), getMoveByName)
import Game.Pokemon (Pokemon (..), getPokemonById)
import Game.Trainer (AIDifficulty, Trainer (..))
import Game.Types (PokemonType (..), Stats (..))

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

data Status = Healthy | Fainted | Paralyzed | Burned | Frozen | Sleep | Poisoned
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

executeTurn :: BattleState -> BattleAction -> BattleAction -> BattleState
executeTurn bState playerAction enemyAction =
  let inTurn = bState {phase = TurnExecution}
      -- TODO: Resolver orden de acciones, cambios, daño, KOs y fin de batalla usando Game.Logic.
      newLog =
        battleLog inTurn
          ++ [ "TODO turn resolution | player=" ++ show playerAction ++ " enemy=" ++ show enemyAction
             ]
   in inTurn
        { phase = WaitingForCommand,
          turnCount = turnCount inTurn + 1,
          battleLog = newLog
        }
