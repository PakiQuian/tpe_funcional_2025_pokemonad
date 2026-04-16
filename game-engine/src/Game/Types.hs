module Game.Types where

-- ==========================================
-- TIPOS ELEMENTALES
-- ==========================================
data PokemonType
  = Fire
  | Water
  | Grass
  | Normal
  | Electric
  | Bug
  | Flying
  | Poison
  | Ground
  | Rock
  | Fighting
  | Psychic
  | Ghost
  | Ice
  | Dragon
  | Steel
  | Fairy
  | Dark
  deriving (Show, Eq)

-- ==========================================
-- ESTADISTICAS
-- ==========================================
data Stats = Stats
  { hp :: Int,
    attack :: Int,
    defense :: Int,
    specialAttack :: Int,
    specialDefense :: Int,
    speed :: Int
  }
  deriving (Show, Eq)

-- ==========================================
-- ESTADO DE BATALLA
-- ==========================================
data Status = Healthy | Fainted | Paralyzed | Burned | Frozen | Asleep | Poisoned
  deriving (Show, Eq)
