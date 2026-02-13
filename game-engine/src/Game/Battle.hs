module Game.Battle where

import Game.Move (Move (..), getMoveByName)
import Game.Pokemon (Pokemon (..), getPokemonById)
import Game.Trainer (Trainer (..))
import Game.Types (PokemonType (..), Stats (..))

-- ==========================================
-- 1. POKEMON DE BATALLA (Dinámico)
-- ==========================================
-- A diferencia del 'Pokemon' estático, este tiene HP actual y Movimientos
data BattlePokemon = BattlePokemon
  { bpOriginal :: Pokemon, -- Datos base (Sprite, Nombre)
    bpHp :: Int, -- Vida Actual
    bpMaxHp :: Int, -- Vida Máxima
    bpMoves :: [Move], -- Sus 4 ataques
    bpStatus :: Status, -- Estado (Healthy, Paralyzed...)
    bpLevel :: Int -- Nivel (fijo en 50 por ahora)
  }
  deriving (Show, Eq)

data Status = Healthy | Fainted | Paralyzed | Burned | Frozen | Sleep | Poisoned
  deriving (Show, Eq)

-- ==========================================
-- 2. ESTADO GLOBAL DE LA BATALLA
-- ==========================================
data BattlePhase
  = WaitingForCommand -- Esperando input del jugador
  | TurnExecution -- Ejecutando animaciones/lógica
  | BattleEnded Winner -- Terminó
  deriving (Show, Eq)

data Winner = PlayerWon | EnemyWon
  deriving (Show, Eq)

data BattleState = BattleState
  { playerActive :: BattlePokemon,
    playerBench :: [BattlePokemon],
    enemyActive :: BattlePokemon,
    enemyBench :: [BattlePokemon],
    turnCount :: Int,
    phase :: BattlePhase,
    battleLog :: [String]
  }
  deriving (Show, Eq)

-- ==========================================
-- 3. INICIALIZACIÓN
-- ==========================================

-- Crea una batalla inicial
initBattle :: [Int] -> Trainer -> BattleState
initBattle playerTeamIds enemyTrainer =
  let -- Construimos los equipos
      pTeam = map makeBattlePokemon playerTeamIds
      eTeam = map makeBattlePokemon (tTeamIds enemyTrainer)
   in BattleState
        { playerActive = head pTeam,
          playerBench = tail pTeam,
          enemyActive = head eTeam,
          enemyBench = tail eTeam,
          turnCount = 1,
          phase = WaitingForCommand,
          battleLog = ["Battle started! Trainer " ++ tName enemyTrainer ++ " wants to battle!"]
        }

-- Constructor de BattlePokemon
-- Aquí asignamos los ataques según el Pokemon (Hardcodeado por ahora)
makeBattlePokemon :: Int -> BattlePokemon
makeBattlePokemon pid =
  case getPokemonById pid of
    Just p ->
      let lvl = 50
          mxHp = calculateHp (hp (pStats p)) lvl
          moves = assignMoves pid (pType p) -- <--- Asignación de ataques
       in BattlePokemon
            { bpOriginal = p,
              bpHp = mxHp,
              bpMaxHp = mxHp,
              bpMoves = moves,
              bpStatus = Healthy,
              bpLevel = lvl
            }
    Nothing -> makeBattlePokemon 25 -- Fallback a Pikachu

-- ==========================================
-- 4. LOGICA DE ASIGNACIÓN DE MOVIMIENTOS
-- ==========================================
-- Como no tenemos base de datos de "Moveset por Nivel", asignamos
-- movimientos temáticos según el Tipo del Pokemon o su ID.

assignMoves :: Int -> [PokemonType] -> [Move]
assignMoves pid types
  -- Casos Especiales (Legendarios / Favoritos)
  | pid == 6 = map getMoveByName ["Flamethrower", "Wing Attack", "Scratch", "Fire Blast"] -- Charizard
  | pid == 9 = map getMoveByName ["Hydro Pump", "Body Slam", "Ice Beam", "Tackle"] -- Blastoise
  | pid == 3 = map getMoveByName ["Solar Beam", "Body Slam", "Razor Leaf", "Tackle"] -- Venusaur
  | pid == 25 = map getMoveByName ["Thunderbolt", "Quick Attack", "Thunder", "Tackle"] -- Pikachu
  | pid == 150 = map getMoveByName ["Psystrike", "Psychic", "Hyper Beam", "Ice Beam"] -- Mewtwo
  | pid == 149 = map getMoveByName ["Dragon Claw", "Hyper Beam", "Thunder", "Fly"] -- Dragonite
  | pid == 143 = map getMoveByName ["Body Slam", "Hyper Beam", "Rest", "Earthquake"] -- Snorlax

  -- Asignación Genérica por Tipo (Si no es especial)
  | Fire `elem` types = map getMoveByName ["Ember", "Scratch", "Tackle", "Flamethrower"]
  | Water `elem` types = map getMoveByName ["Water Gun", "Tackle", "Surf", "Ice Beam"]
  | Grass `elem` types = map getMoveByName ["Vine Whip", "Tackle", "Razor Leaf", "Body Slam"]
  | Electric `elem` types = map getMoveByName ["Thundershock", "Tackle", "Thunderbolt", "Quick Attack"]
  | Psychic `elem` types = map getMoveByName ["Confusion", "Psychic", "Body Slam", "Tackle"]
  | Rock `elem` types = map getMoveByName ["Rock Slide", "Earthquake", "Tackle", "Body Slam"]
  | Ground `elem` types = map getMoveByName ["Earthquake", "Rock Slide", "Scratch", "Tackle"]
  | otherwise = map getMoveByName ["Tackle", "Scratch", "Body Slam", "Quick Attack"]

-- Fórmula de HP simplificada
calculateHp :: Int -> Int -> Int
calculateHp baseStat level =
  (((baseStat + 31) * 2 * level) `div` 100) + level + 10