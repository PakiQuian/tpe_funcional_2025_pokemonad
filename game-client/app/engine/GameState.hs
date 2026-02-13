module Engine.GameState
  ( Screen (..),
    GameState (..),
    BattleMenuType (..),
  )
where

import qualified Data.Map as Map
import Game.Battle (BattleState)
import Game.Trainer (Trainer)
import Graphics.Gloss (Picture)
import System.Random (StdGen)

data Screen
  = StartScreen
  | Menu
  | Pokedex
  | PokemonDetail
  | Multiplayer
  | TeamSelect
  | OpponentSelect
  | BattleScreen
  deriving (Show, Eq)

data BattleMenuType = MainBattleMenu | FightMenu | BagMenu | PokemonMenu
  deriving (Show, Eq)

data GameState = GameState
  { currentScreen :: Screen,
    selectedOption :: Int,
    selectedPokemon :: Int,
    playerTeam :: [Int],
    selectedTrainer :: Maybe Trainer,
    selectedTrainerIndex :: Int,
    startBgImage :: Picture,
    menuBgImage :: Picture,
    logoImage :: Picture,
    rngSeed :: StdGen,
    holdingUp :: Bool,
    holdingDown :: Bool,
    scrollTimer :: Float,
    -- Sprites
    pokemonFrontSprites :: Map.Map Int Picture,
    pokemonBackSprites :: Map.Map Int Picture,
    trainerSprites :: Map.Map Int Picture,
    -- Campos de Batalla
    battleState :: Maybe BattleState,
    battleBackgrounds :: [Picture],
    currentBattleBg :: Int,
    battleMenuIndex :: Int,
    -- Campos para el submenu
    battleMenuType :: BattleMenuType,
    battleMoveIndex :: Int
  }