module Engine.World
  ( NetSubState (..),
    World (..),
    drainNetInbox,
    applyNetMsg,
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue)
import Data.List (foldl')
import Engine.GameState (GameState)
import P2P.Serialization (AppMsg (..))

-- Posible estados de la subfase de red.
data NetSubState
  = NetDisconnected
  | NetListening Int
  | NetConnecting String Int
  | NetInLobby
  | NetInBattle
  deriving (Eq, Show)

-- Estado de la ventana Gloss + canal de entrada de mensajes de red
data World = World
  { worldGame :: GameState,
    netInQueue :: TQueue AppMsg,
    netSubState :: NetSubState
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
