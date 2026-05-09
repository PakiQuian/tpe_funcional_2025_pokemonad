module Client.State
  ( GameState (..),
    World (..),
    initialState,
    drainNetInbox,
    applyNetMsg,
    mergeNetAsync,
    disconnectNetWorld,
  )
where

import Client.Types
  ( AITrainingResult (..),
    BattleMenuType (..),
    MultiplayerIntent,
    NetConnAsync (..),
    NetSubState (..),
    Screen (..),
  )
import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue)
import Control.Concurrent.STM.TVar (TVar)
import Data.List (foldl')
import qualified Data.Map as Map
import Graphics.Gloss (Picture)
import Network.Socket (Socket, close)
import P2P.Types (AppMsg (..))
import Pokemonad.AI.Model (QWeights)
import Pokemonad.Battle.State (BattleState)
import Pokemonad.Core.Trainer (Trainer)
import Pokemonad.Core.Types (PokemonId (..), TrainerId (..))
import System.Random (StdGen)

data GameState = GameState
  { currentScreen :: Screen,
    selectedOption :: Int,
    selectedPokemonId :: PokemonId,
    playerTeam :: [PokemonId],
    selectedTrainer :: Maybe Trainer,
    selectedTrainerIndex :: Int,
    startBgImage :: Picture,
    menuBgImage :: Picture,
    logoImage :: Picture,
    winnerBgImage :: Picture,
    loserBgImage :: Picture,
    randomGen :: StdGen,
    holdingUp :: Bool,
    holdingDown :: Bool,
    scrollTimer :: Float,
    pokemonFrontSprites :: Map.Map PokemonId Picture,
    pokemonBackSprites :: Map.Map PokemonId Picture,
    trainerSprites :: Map.Map TrainerId Picture,
    battleState :: Maybe BattleState,
    battleBackgrounds :: [Picture],
    currentBattleBg :: Int,
    battleMenuIndex :: Int,
    battleMenuType :: BattleMenuType,
    battleMoveIndex :: Int,
    battleBenchIndex :: Int,
    multiplayerHost :: String,
    multiplayerPort :: String,
    multiplayerRow :: Int,
    multiplayerPending :: Maybe MultiplayerIntent,
    multiplayerError :: Maybe String,
    enemyAIWeights :: Maybe QWeights,
    simulatorTraining :: Bool,
    simulatorStatus :: String,
    simulatorTotalEpochs :: Int,
    simulatorLogs :: [String]
  }

data World = World
  { worldGame :: GameState,
    netInQueue :: TQueue AppMsg,
    netSubState :: NetSubState,
    netSocket :: Maybe Socket,
    netConnAsync :: TVar (Maybe NetConnAsync),
    aiTrainingAsync :: TVar (Maybe AITrainingResult)
  }

initialState ::
  Picture ->
  Picture ->
  Picture ->
  Picture ->
  Picture ->
  Map.Map PokemonId Picture ->
  Map.Map PokemonId Picture ->
  Map.Map TrainerId Picture ->
  [Picture] ->
  StdGen ->
  GameState
initialState startBg menuBg logo winnerBg loserBg frontSprites backSprites trSprites battleBgs rng =
  GameState
    { currentScreen = StartScreen,
      selectedOption = 0,
      selectedPokemonId = PokemonId 1,
      playerTeam = [],
      selectedTrainer = Nothing,
      selectedTrainerIndex = 0,
      startBgImage = startBg,
      menuBgImage = menuBg,
      logoImage = logo,
      winnerBgImage = winnerBg,
      loserBgImage = loserBg,
      randomGen = rng,
      holdingUp = False,
      holdingDown = False,
      scrollTimer = 0.0,
      pokemonFrontSprites = frontSprites,
      pokemonBackSprites = backSprites,
      trainerSprites = trSprites,
      battleState = Nothing,
      battleBackgrounds = battleBgs,
      currentBattleBg = 0,
      battleMenuIndex = 0,
      battleMenuType = MainBattleMenu,
      battleMoveIndex = 0,
      battleBenchIndex = 0,
      multiplayerHost = "127.0.0.1",
      multiplayerPort = "7878",
      multiplayerRow = 0,
      multiplayerPending = Nothing,
      multiplayerError = Nothing,
      enemyAIWeights = Nothing,
      simulatorTraining = False,
      simulatorStatus = "Ready. Press ENTER to run 100 epochs.",
      simulatorTotalEpochs = 0,
      simulatorLogs = []
    }

drainNetInbox :: World -> IO World
drainNetInbox w = do
  msgs <- atomically $ drainAll (netInQueue w)
  pure $ foldl' applyNetMsgToWorld w msgs

drainAll :: TQueue a -> STM [a]
drainAll q = go []
  where
    go acc = do
      mx <- tryReadTQueue q
      case mx of
        Nothing -> pure (reverse acc)
        Just x -> go (x : acc)

applyNetMsgToWorld :: World -> AppMsg -> World
applyNetMsgToWorld w msg =
  w
    { worldGame = applyNetMsg msg (worldGame w),
      netSubState = netSubStateAfterMsg (netSubState w) msg
    }

applyNetMsg :: AppMsg -> GameState -> GameState
applyNetMsg _ gs = gs

netSubStateAfterMsg :: NetSubState -> AppMsg -> NetSubState
netSubStateAfterMsg NetInLobby AppMsgBattleReady = NetInBattle
netSubStateAfterMsg s _ = s

mergeNetAsync :: World -> IO World
mergeNetAsync w = do
  m <- atomically $ do
    x <- readTVar (netConnAsync w)
    writeTVar (netConnAsync w) Nothing
    pure x
  case m of
    Nothing -> pure w
    Just (NetConnErr err) ->
      pure w {netSubState = NetDisconnected, worldGame = (worldGame w) {multiplayerError = Just err}}
    Just (NetConnOk sock st) ->
      if currentScreen (worldGame w) /= Multiplayer
        then do
          close sock
          pure w
        else
          pure
            w
              { netSocket = Just sock,
                netSubState = st,
                worldGame = (worldGame w) {multiplayerError = Nothing}
              }

disconnectNetWorld :: World -> IO World
disconnectNetWorld w =
  case netSocket w of
    Just sock -> do
      close sock
      pure w {netSocket = Nothing, netSubState = NetDisconnected}
    Nothing -> pure w {netSubState = NetDisconnected}
