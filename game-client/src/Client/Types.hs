module Client.Types
  ( Screen (..),
    BattleMenuType (..),
    MultiplayerIntent (..),
    AITrainingResult (..),
    NetSubState (..),
    NetConnAsync (..),
  )
where

import Network.Socket (HostName, PortNumber, Socket)
import Pokemonad.AI.Model (QWeights)
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
  | BagMenu
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
  = NetConnOk Socket NetSubState
  | NetConnErr String
  deriving (Eq, Show)
