module Client.State
  ( AppState (..),
    World (..),
    initialState,
    drainNetInbox,
    applyNetMsg,
    mergeNetAsync,
    mergeMultiplayerLobby,
    mergeMultiplayerBattle,
    disconnectNetWorld,
  )
where

import Client.NetSerializers ()
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
import Control.Exception (SomeException, try)
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import qualified Data.Map as Map
import Graphics.Gloss (Picture)
import Network.Socket (Socket, close)
import P2P.Communication (sendMsg)
import P2P.Types (AppMsg (..), PlayerAction (..))
import Pokemonad.AI.Model (QWeights)
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattleState (..),
    flipBattleState,
    flipBattleStep,
    initBattleFromTeams,
  )
import Pokemonad.Battle.Turn (BattleStep, executeTurnMulti)
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
    aiTrainingAsync :: TVar (Maybe AITrainingResult),
    netIsHost :: Bool
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
applyNetMsg (AppMsgTeam ids) gs =
  let pids = map (PokemonId . fromIntegral) ids
      ms = multiplayerState gs
   in gs {multiplayerState = ms {mpOpponentTeam = Just pids}}
applyNetMsg (AppMsgAction action) gs =
  let bss = battleScreenState gs
      ba = case action of
        UseMove idx -> ActionMove idx
        SwitchPokemon idx -> ActionSwitch idx
   in gs {battleScreenState = bss {battlePendingRemoteAction = Just ba}}
applyNetMsg (AppMsgBattleState bs) gs =
  let battleSt = decode (BL.fromStrict bs) :: BattleState
      menuType = case phase battleSt of
        WaitingForForcedPlayerSwitch -> PokemonMenu
        _ -> MainBattleMenu
      bss =
        (battleScreenState gs)
          { currentBattle = Just battleSt,
            battleIsMultiplayer = True,
            battleMenuType = menuType,
            battlePendingLocalAction = Nothing,
            battlePendingRemoteAction = Nothing,
            battlePendingFrames = [],
            battleFrameTimer = 0,
            battleShakeTimer = 0,
            battleShakeTarget = Nothing
          }
   in gs {currentScreen = BattleScreen, battleScreenState = bss}
applyNetMsg (AppMsgBattleFrames bs) gs =
  let frames = decode (BL.fromStrict bs) :: [BattleStep]
      bss0 = battleScreenState gs
      bss =
        bss0
          { battlePendingFrames = frames,
            battleFrameTimer = 0.6,
            battlePendingLocalAction = Nothing,
            battlePendingRemoteAction = Nothing
          }
   in gs {battleScreenState = bss}
applyNetMsg AppMsgDisconnect gs =
  let ms = multiplayerState gs
   in gs
        { currentScreen = Multiplayer,
          battleScreenState = defaultBattleScreenState,
          multiplayerState =
            defaultMultiplayerState
              { mpError = Just "Opponent disconnected."
              }
        }
applyNetMsg _ gs = gs

netSubStateAfterMsg :: NetSubState -> AppMsg -> NetSubState
netSubStateAfterMsg NetInLobby AppMsgBattleReady = NetInBattle
netSubStateAfterMsg NetInLobby (AppMsgBattleState _) = NetInBattle
netSubStateAfterMsg _ AppMsgDisconnect = NetDisconnected
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
    Just (NetConnOk sock st isHost) ->
      if currentScreen (worldGame w) /= Multiplayer
        then do
          close sock
          pure w
        else
          pure
            w
              { netSocket = Just sock,
                netSubState = st,
                netIsHost = isHost,
                worldGame =
                  (worldGame w)
                    { multiplayerState = (multiplayerState (worldGame w)) {mpError = Nothing}
                    }
              }

mergeMultiplayerLobby :: World -> IO World
mergeMultiplayerLobby w
  | not (netIsHost w) = pure w
  | netSubState w /= NetInLobby = pure w
  | otherwise =
      let gs = worldGame w
          ms = multiplayerState gs
       in case (mpTeamSent ms, mpOpponentTeam ms) of
            (True, Just oppTeamIds) -> do
              let myTeamIds = playerTeam gs
                  battleSt = initBattleFromTeams myTeamIds oppTeamIds
                  flipped = flipBattleState battleSt
                  encoded = BL.toStrict (encode flipped)
              case netSocket w of
                Just sock -> sendMsg sock (AppMsgBattleState encoded)
                Nothing -> pure ()
              let bss =
                    defaultBattleScreenState
                      { currentBattle = Just battleSt,
                        battleIsMultiplayer = True,
                        battleMenuType = MainBattleMenu
                      }
                  newMs = ms {mpTeamSent = False, mpOpponentTeam = Nothing}
                  newGs =
                    gs
                      { currentScreen = BattleScreen,
                        battleScreenState = bss,
                        multiplayerState = newMs
                      }
              pure w {netSubState = NetInBattle, worldGame = newGs}
            _ -> pure w

mergeMultiplayerBattle :: World -> IO World
mergeMultiplayerBattle w
  | not (netIsHost w) = pure w
  | netSubState w /= NetInBattle = pure w
  | otherwise =
      let gs = worldGame w
          bss = battleScreenState gs
       in case currentBattle bss of
            Nothing -> pure w
            Just battleSt ->
              case phase battleSt of
                WaitingForForcedEnemySwitch ->
                  case battlePendingRemoteAction bss of
                    Just remoteAction -> executeMPTurn w gs bss battleSt (ActionMove 0) remoteAction
                    Nothing -> pure w
                WaitingForForcedPlayerSwitch ->
                  case battlePendingLocalAction bss of
                    Just localAction -> executeMPTurn w gs bss battleSt localAction (ActionMove 0)
                    Nothing -> pure w
                WaitingForCommand ->
                  case (battlePendingLocalAction bss, battlePendingRemoteAction bss) of
                    (Just localAction, Just remoteAction) ->
                      executeMPTurn w gs bss battleSt localAction remoteAction
                    _ -> pure w
                _ -> pure w

executeMPTurn ::
  World ->
  AppState ->
  BattleScreenState ->
  BattleState ->
  BattleAction ->
  BattleAction ->
  IO World
executeMPTurn w gs bss battleSt localAction remoteAction = do
  let rng = randomGen gs
      (frames, newRng) = executeTurnMulti rng battleSt localAction remoteAction
      framesForPeer = map flipBattleStep frames
      encoded = BL.toStrict (encode framesForPeer)
  case netSocket w of
    Just sock -> sendMsg sock (AppMsgBattleFrames encoded)
    Nothing -> pure ()
  let newBss =
        bss
          { battlePendingFrames = frames,
            battleFrameTimer = 0.6,
            battlePendingLocalAction = Nothing,
            battlePendingRemoteAction = Nothing
          }
      newGs =
        gs
          { battleScreenState = newBss,
            randomGen = newRng
          }
  pure w {worldGame = newGs}

disconnectNetWorld :: World -> IO World
disconnectNetWorld w =
  case netSocket w of
    Just sock -> do
      _ <- try (sendMsg sock AppMsgDisconnect) :: IO (Either SomeException ())
      close sock
      pure w {netSocket = Nothing, netSubState = NetDisconnected, netIsHost = False}
    Nothing -> pure w {netSubState = NetDisconnected, netIsHost = False}
