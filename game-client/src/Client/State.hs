module Client.State
  ( AppState (..),
    World (..),
    initialState,
    drainNetInbox,
    applyNetMsg,
    mergeNetAsync,
    disconnectNetWorld,
  )
where

import Client.Types
  ( AISimulatorState (..),
    AITrainingResult (..),
    Assets (..),
    BattleMenuType (..),
    BattleScreenState (..),
    MenuState (..),
    MultiplayerIntent,
    MultiplayerState (..),
    NetConnAsync (..),
    NetSubState (..),
    OpponentSelectState (..),
    PokedexState (..),
    Screen (..),
    TeamSelectState (..),
    defaultBattleScreenState,
    defaultMultiplayerState,
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
import Pokemonad.Core.Trainer (Trainer)
import Pokemonad.Core.Types (PokemonId (..), TrainerId (..))
import System.Random (StdGen)

data AppState = AppState
  { currentScreen :: Screen,
    randomGen :: StdGen,
    playerTeam :: [PokemonId],
    selectedTrainer :: Maybe Trainer,
    enemyAIWeights :: Maybe QWeights,
    holdingUp :: Bool,
    holdingDown :: Bool,
    scrollTimer :: Float,
    assets :: Assets,
    menuState :: MenuState,
    pokedexState :: PokedexState,
    teamSelectState :: TeamSelectState,
    opponentState :: OpponentSelectState,
    multiplayerState :: MultiplayerState,
    battleScreenState :: BattleScreenState,
    aiSimState :: AISimulatorState
  }

data World = World
  { worldGame :: AppState,
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
  AppState
initialState startBg menuBg logo winnerBg loserBg frontSprites backSprites trSprites battleBgs rng =
  AppState
    { currentScreen = StartScreen,
      randomGen = rng,
      playerTeam = [],
      selectedTrainer = Nothing,
      enemyAIWeights = Nothing,
      holdingUp = False,
      holdingDown = False,
      scrollTimer = 0.0,
      assets =
        Assets
          { assetStartBg = startBg,
            assetMenuBg = menuBg,
            assetLogo = logo,
            assetWinnerBg = winnerBg,
            assetLoserBg = loserBg,
            assetBattleBgs = battleBgs,
            assetPokeFront = frontSprites,
            assetPokeBack = backSprites,
            assetTrainers = trSprites
          },
      menuState = MenuState {menuCursor = 0},
      pokedexState = PokedexState {pokedexCursor = PokemonId 1},
      teamSelectState = TeamSelectState {teamSelectCursor = PokemonId 1},
      opponentState = OpponentSelectState {trainerCursor = 0},
      multiplayerState = defaultMultiplayerState,
      battleScreenState = defaultBattleScreenState,
      aiSimState =
        AISimulatorState
          { aiTraining = False,
            aiStatus = "Ready. Press ENTER to run 100 epochs.",
            aiTotalEpochs = 0,
            aiLogs = []
          }
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

applyNetMsg :: AppMsg -> AppState -> AppState
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
      pure
        w
          { netSubState = NetDisconnected,
            worldGame =
              (worldGame w)
                { multiplayerState = (multiplayerState (worldGame w)) {mpError = Just err}
                }
          }
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
                worldGame =
                  (worldGame w)
                    { multiplayerState = (multiplayerState (worldGame w)) {mpError = Nothing}
                    }
              }

disconnectNetWorld :: World -> IO World
disconnectNetWorld w =
  case netSocket w of
    Just sock -> do
      close sock
      pure w {netSocket = Nothing, netSubState = NetDisconnected}
    Nothing -> pure w {netSubState = NetDisconnected}
