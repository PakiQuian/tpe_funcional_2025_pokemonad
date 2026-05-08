module Engine.GameState
  ( Screen (..),
    GameState (..),
    BattleMenuType (..),
    MultiplayerIntent (..),
    AITrainingResult (..),
  )
where

import qualified Data.Map as Map
import Game.AI (QWeights)
import Game.Battle (BattleState)
import Game.Trainer (Trainer)
import Graphics.Gloss (Picture)
import Network.Socket (HostName, PortNumber)
import System.Random (StdGen)

data AITrainingResult = AITrainingResult
  { atrWeights :: QWeights,
    atrTotalEpochs :: Int,
    atrLogs :: [String],
    atrStatus :: String,
    atrRng :: StdGen
  }

-- | Acción de red solicitada desde la pantalla multijugador; Main la ejecuta en IO.
data MultiplayerIntent
  = MPListen PortNumber
  | MPConnect HostName PortNumber
  deriving (Eq, Show)

data Screen
  = StartScreen
  | Menu
  | Pokedex
  | PokemonDetail
  | Multiplayer
  | AISimulator
  | TeamSelect
  | OpponentSelect
  | BattleScreen
  | BattleResultScreen
  deriving (Show, Eq)

data BattleMenuType = MainBattleMenu | FightMenu | BagMenu | PokemonMenu | SwitchConfirmMenu | QuitConfirmMenu
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
    winnerBgImage :: Picture,
    loserBgImage :: Picture,
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
    battleMoveIndex :: Int,
    battleBenchIndex :: Int,
    -- Multijugador P2P (host / puerto / acciones)
    multiplayerHost :: String,
    multiplayerPort :: String,
    -- 0 = editar host, 1 = editar puerto, 2 = escuchar, 3 = conectar
    multiplayerRow :: Int,
    multiplayerPending :: Maybe MultiplayerIntent,
    multiplayerError :: Maybe String,
    enemyAIWeights :: Maybe QWeights,
    simulatorTraining :: Bool,
    simulatorStatus :: String,
    simulatorTotalEpochs :: Int,
    simulatorLogs :: [String]
  }