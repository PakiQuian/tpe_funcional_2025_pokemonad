{-# OPTIONS_GHC -Wno-orphans #-}

module Client.NetSerializers () where

import Data.Binary (Binary (..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
    Side (..),
    Winner (..),
  )
import Pokemonad.Core.Move (Move (..), MoveCategory (..))
import Pokemonad.Core.Pokemon (Pokemon (..))
import Pokemonad.Core.Trainer (AIDifficulty (..))
import Pokemonad.Core.Types
  ( HP (..),
    Level (..),
    PokemonId (..),
    PokemonType (..),
    Stats (..),
    Status (..),
    TrainerId (..),
  )

instance Binary HP where
  put (HP n) = put n
  get = HP <$> get

instance Binary Level where
  put (Level n) = put n
  get = Level <$> get

instance Binary PokemonId where
  put (PokemonId n) = put n
  get = PokemonId <$> get

instance Binary TrainerId where
  put (TrainerId n) = put n
  get = TrainerId <$> get

instance Binary PokemonType where
  put t = putWord8 $ case t of
    Fire -> 0; Water -> 1; Grass -> 2; Normal -> 3; Electric -> 4
    Bug -> 5; Flying -> 6; Poison -> 7; Ground -> 8; Rock -> 9
    Fighting -> 10; Psychic -> 11; Ghost -> 12; Ice -> 13; Dragon -> 14
    Steel -> 15; Fairy -> 16; Dark -> 17
  get = do
    tag <- getWord8
    case tag of
      0 -> pure Fire; 1 -> pure Water; 2 -> pure Grass; 3 -> pure Normal
      4 -> pure Electric; 5 -> pure Bug; 6 -> pure Flying; 7 -> pure Poison
      8 -> pure Ground; 9 -> pure Rock; 10 -> pure Fighting; 11 -> pure Psychic
      12 -> pure Ghost; 13 -> pure Ice; 14 -> pure Dragon; 15 -> pure Steel
      16 -> pure Fairy; 17 -> pure Dark
      _ -> fail "NetSerializers: unknown PokemonType"

instance Binary Stats where
  put (Stats hp atk def spa spd spe) =
    put hp >> put atk >> put def >> put spa >> put spd >> put spe
  get = Stats <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary Status where
  put s = putWord8 $ case s of
    Healthy -> 0; Fainted -> 1; Paralyzed -> 2; Burned -> 3
    Frozen -> 4; Asleep -> 5; Poisoned -> 6
  get = do
    tag <- getWord8
    case tag of
      0 -> pure Healthy; 1 -> pure Fainted; 2 -> pure Paralyzed; 3 -> pure Burned
      4 -> pure Frozen; 5 -> pure Asleep; 6 -> pure Poisoned
      _ -> fail "NetSerializers: unknown Status"

instance Binary MoveCategory where
  put c = putWord8 $ case c of
    Physical -> 0; Special -> 1; Status -> 2
  get = do
    tag <- getWord8
    case tag of
      0 -> pure Physical; 1 -> pure Special; 2 -> pure Status
      _ -> fail "NetSerializers: unknown MoveCategory"

instance Binary Move where
  put m =
    put (moveName m) >> put (moveType m) >> put (moveCategory m)
      >> put (movePower m) >> put (moveAccuracy m) >> put (movePP m) >> put (moveMaxPP m)
  get = Move <$> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Binary Pokemon where
  put p =
    put (pokemonId p) >> put (pokemonName p) >> put (pokemonTypes p)
      >> put (pokemonStats p) >> put (pokemonDescription p) >> put (pokemonMoves p)
  get = Pokemon <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary AIDifficulty where
  put d = putWord8 $ case d of
    DifficultyEasy -> 0; DifficultyMedium -> 1; DifficultyHard -> 2; DifficultyExtreme -> 3
  get = do
    tag <- getWord8
    case tag of
      0 -> pure DifficultyEasy; 1 -> pure DifficultyMedium
      2 -> pure DifficultyHard; 3 -> pure DifficultyExtreme
      _ -> fail "NetSerializers: unknown AIDifficulty"

instance Binary Winner where
  put PlayerWon = putWord8 0
  put EnemyWon = putWord8 1
  get = do
    tag <- getWord8
    case tag of
      0 -> pure PlayerWon; 1 -> pure EnemyWon
      _ -> fail "NetSerializers: unknown Winner"

instance Binary BattlePhase where
  put p = case p of
    WaitingForCommand -> putWord8 0
    TurnExecution -> putWord8 1
    WaitingForForcedPlayerSwitch -> putWord8 2
    WaitingForForcedEnemySwitch -> putWord8 3
    BattleEnded w -> putWord8 4 >> put w
  get = do
    tag <- getWord8
    case tag of
      0 -> pure WaitingForCommand
      1 -> pure TurnExecution
      2 -> pure WaitingForForcedPlayerSwitch
      3 -> pure WaitingForForcedEnemySwitch
      4 -> BattleEnded <$> get
      _ -> fail "NetSerializers: unknown BattlePhase"

instance Binary Side where
  put PlayerSide = putWord8 0
  put EnemySide = putWord8 1
  get = do
    tag <- getWord8
    case tag of
      0 -> pure PlayerSide; 1 -> pure EnemySide
      _ -> fail "NetSerializers: unknown Side"

instance Binary BattleAction where
  put a = case a of
    ActionMove idx -> putWord8 0 >> put idx
    ActionSwitch idx -> putWord8 1 >> put idx
  get = do
    tag <- getWord8
    case tag of
      0 -> ActionMove <$> get
      1 -> ActionSwitch <$> get
      _ -> fail "NetSerializers: unknown BattleAction"

instance Binary BattlePokemon where
  put bp =
    put (battlePokemonBase bp) >> put (battlePokemonHp bp) >> put (battlePokemonMaxHp bp)
      >> put (battlePokemonMoves bp) >> put (battlePokemonStatus bp) >> put (battlePokemonLevel bp)
  get = BattlePokemon <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary BattleState where
  put bs =
    put (playerActive bs) >> put (playerBench bs) >> put (enemyActive bs)
      >> put (enemyBench bs) >> put (enemyDifficulty bs) >> put (turnCount bs)
      >> put (phase bs) >> put (battleLog bs)
  get = BattleState <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
