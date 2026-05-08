module Engine.World
  ( NetSubState (..),
    NetConnAsync (..),
    World (..),
    drainNetInbox,
    applyNetMsg,
    mergeNetAsync,
    disconnectNetWorld,
  )
where

import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue)
import Control.Concurrent.STM.TVar (TVar)
import Data.List (foldl')
import Engine.GameState (AITrainingResult, GameState (..), Screen (..))
import Network.Socket (Socket, close)
import P2P.Serialization (AppMsg (..))

-- Posible estados de la subfase de red.
data NetSubState
  = NetDisconnected
  | NetListening Int
  | NetConnecting String Int
  | NetInLobby
  | NetInBattle
  deriving (Eq, Show)

-- | Resultado de listen/connect en un hilo auxiliar; el hilo principal lo fusiona en 'handleWorldTick'.
data NetConnAsync
  = NetConnOk Socket NetSubState
  | NetConnErr String
  deriving (Eq, Show)

-- Estado de la ventana Gloss + canal de entrada de mensajes de red
data World = World
  { worldGame :: GameState,
    netInQueue :: TQueue AppMsg,
    netSubState :: NetSubState,
    netSocket :: Maybe Socket,
    netConnAsync :: TVar (Maybe NetConnAsync),
    aiTrainingAsync :: TVar (Maybe AITrainingResult)
  }

-- Vacía la cola en una sola transacción STM y aplica los mensajes al juego.
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

-- TODO: Actualizar 'applyNetMsg' para que los mensajes de red modifiquen 'GameState' cuando exista batalla P2P.
applyNetMsg :: AppMsg -> GameState -> GameState
applyNetMsg _ gs = gs

netSubStateAfterMsg :: NetSubState -> AppMsg -> NetSubState
netSubStateAfterMsg NetInLobby AppMsgBattleReady = NetInBattle
netSubStateAfterMsg s _ = s

-- | Incorpora un resultado asíncrono de conexión (si el jugador sigue en multijugador).
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
            worldGame = (worldGame w) {multiplayerError = Just err}
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
                worldGame = (worldGame w) {multiplayerError = Nothing}
              }

-- | Cierra el socket activo y vuelve el subestado a desconectado.
disconnectNetWorld :: World -> IO World
disconnectNetWorld w =
  case netSocket w of
    Just sock -> do
      close sock
      pure
        w
          { netSocket = Nothing,
            netSubState = NetDisconnected
          }
    Nothing -> pure w {netSubState = NetDisconnected}
