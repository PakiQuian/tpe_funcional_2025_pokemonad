module Pokemonad.Core.Types
  ( PokemonType (..),
    Stats (..),
    Status (..),
    HP (..),
    Level (..),
    PokemonId (..),
    TrainerId (..),
  )
where

newtype HP = HP {unHP :: Int}
  deriving (Show, Eq, Ord)

newtype Level = Level {unLevel :: Int}
  deriving (Show, Eq, Ord)

newtype PokemonId = PokemonId {unPokemonId :: Int}
  deriving (Show, Eq, Ord)

newtype TrainerId = TrainerId {unTrainerId :: Int}
  deriving (Show, Eq, Ord)

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

data Stats = Stats
  { statsHp :: Int,
    statsAttack :: Int,
    statsDefense :: Int,
    statsSpecialAttack :: Int,
    statsSpecialDefense :: Int,
    statsSpeed :: Int
  }
  deriving (Show, Eq)

data Status = Healthy | Fainted | Paralyzed | Burned | Frozen | Asleep | Poisoned
  deriving (Show, Eq)
