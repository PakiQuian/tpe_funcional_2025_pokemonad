module Client.Types
  ( Screen (..),
    BattleMenuType (..),
    MultiplayerIntent (..),
    AITrainingResult (..),
    NetSubState (..),
    NetConnAsync (..),
    Assets (..),
    MenuState (..),
    PokedexState (..),
    TeamSelectState (..),
    OpponentSelectState (..),
    MultiplayerState (..),
    BattleScreenState (..),
    AISimulatorState (..),
    defaultBattleScreenState,
    defaultMultiplayerState,
  )
where

import qualified Data.Map as Map
import Graphics.Gloss (Picture)
import Network.Socket (HostName, PortNumber, Socket)
import Pokemonad.AI.Model (QWeights)
import Pokemonad.Battle.State (BattleAction, BattleState, Side)
import Pokemonad.Battle.Turn (BattleStep)
import Pokemonad.Core.Types (PokemonId (..), TrainerId (..))
import System.Random (StdGen)

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

data BattleMenuType
  = MainBattleMenu
  | FightMenu
  | PokemonMenu
  | SwitchConfirmMenu
  | QuitConfirmMenu
  deriving (Show, Eq)

data MultiplayerIntent
  = MPListen PortNumber
  | MPConnect HostName PortNumber
  deriving (Eq, Show)

data AITrainingResult = AITrainingResult
  { atrWeights :: QWeights,
    atrTotalEpochs :: Int,
    atrLogs :: [String],
    atrStatus :: String,
    atrRng :: StdGen
  }

data NetSubState
  = NetDisconnected
  | NetListening Int
  | NetConnecting String Int
  | NetInLobby
  | NetInBattle
  deriving (Eq, Show)

data NetConnAsync
  = NetConnOk Socket NetSubState Bool
  | NetConnErr String
  deriving (Eq, Show)

data Assets = Assets
  { assetStartBg :: Picture,
    assetMenuBg :: Picture,
    assetLogo :: Picture,
    assetWinnerBg :: Picture,
    assetLoserBg :: Picture,
    assetBattleBgs :: [Picture],
    assetPokeFront :: Map.Map PokemonId Picture,
    assetPokeBack :: Map.Map PokemonId Picture,
    assetTrainers :: Map.Map TrainerId Picture
  }

data MenuState = MenuState
  {menuCursor :: Int}

data PokedexState = PokedexState
  {pokedexCursor :: PokemonId}

data TeamSelectState = TeamSelectState
  {teamSelectCursor :: PokemonId}

data OpponentSelectState = OpponentSelectState
  {trainerCursor :: Int}

data MultiplayerState = MultiplayerState
  { mpCursor :: Int,
    mpHost :: String,
    mpPort :: String,
    mpPending :: Maybe MultiplayerIntent,
    mpError :: Maybe String,
    mpTeamSent :: Bool,
    mpOpponentTeam :: Maybe [PokemonId]
  }

data BattleScreenState = BattleScreenState
  { battleMainCursor :: Int,
    battleMoveCursor :: Int,
    battleBenchCursor :: Int,
    battleMenuType :: BattleMenuType,
    currentBattle :: Maybe BattleState,
    battleBgIndex :: Int,
    battleIsMultiplayer :: Bool,
    battlePendingLocalAction :: Maybe BattleAction,
    battlePendingRemoteAction :: Maybe BattleAction,
    battlePendingFrames :: [BattleStep],
    battleFrameTimer :: Float,
    battleShakeTimer :: Float,
    battleShakeTarget :: Maybe Side
  }

data AISimulatorState = AISimulatorState
  { aiTraining :: Bool,
    aiStatus :: String,
    aiTotalEpochs :: Int,
    aiLogs :: [String]
  }

defaultBattleScreenState :: BattleScreenState
defaultBattleScreenState =
  BattleScreenState
    { battleMainCursor = 0,
      battleMoveCursor = 0,
      battleBenchCursor = 0,
      battleMenuType = MainBattleMenu,
      currentBattle = Nothing,
      battleBgIndex = 0,
      battleIsMultiplayer = False,
      battlePendingLocalAction = Nothing,
      battlePendingRemoteAction = Nothing,
      battlePendingFrames = [],
      battleFrameTimer = 0.0,
      battleShakeTimer = 0.0,
      battleShakeTarget = Nothing
    }

defaultMultiplayerState :: MultiplayerState
defaultMultiplayerState =
  MultiplayerState
    { mpCursor = 0,
      mpHost = "127.0.0.1",
      mpPort = "7878",
      mpPending = Nothing,
      mpError = Nothing,
      mpTeamSent = False,
      mpOpponentTeam = Nothing
    }
