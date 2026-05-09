module P2P.Communication
  ( -- conexión
    listenAndAccept,
    connectTo,
    -- mensajes
    sendMsg,
    forkRecvLoop,
    recvLoop,
  )
where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Control.Exception (SomeException, bracketOnError, catch, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import P2P.Serialization (decodeAllFrames, encodeFramed)
import P2P.Types (AppMsg (..))

-- Lecturas pequeñas, el buffer lógico es 'buf' en 'recvLoop' (TCP es flujo continuo).
recvChunkSize :: Int
recvChunkSize = 4096

listenAndAccept :: PortNumber -> IO (Socket, SockAddr)
listenAndAccept port =
  withSocketsDo $ do
    let hints =
          defaultHints
            { addrFlags = [AI_PASSIVE],
              addrSocketType = Stream
            }
    ais <- getAddrInfo (Just hints) Nothing (Just (show port))
    case ais of
      [] -> ioError $ userError "P2P.Communication.listenAndAccept: getAddrInfo returned no addresses"
      (ai : _) ->
        bracketOnError
          (socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai))
          close
          $ \listenSock -> do
            setSocketOption listenSock ReuseAddr 1
            bind listenSock (addrAddress ai)
            listen listenSock 5
            bracketOnError
              (accept listenSock)
              (close . fst)
              $ \(conn, peer) -> do
                close listenSock
                pure (conn, peer)

connectTo :: HostName -> PortNumber -> IO Socket
connectTo host port =
  withSocketsDo $ do
    let hints = defaultHints {addrSocketType = Stream}
    ais <- getAddrInfo (Just hints) (Just host) (Just (show port))
    case ais of
      [] -> ioError $ userError "P2P.Communication.connectTo: getAddrInfo returned no addresses"
      (ai : _) ->
        bracketOnError
          (socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai))
          close
          $ \sock -> do
            connect sock (addrAddress ai)
            pure sock

sendMsg :: Socket -> AppMsg -> IO ()
sendMsg sock msg = NSB.sendAll sock (encodeFramed msg)

forkRecvLoop :: Socket -> TQueue AppMsg -> IO ThreadId
forkRecvLoop sock q =
  forkIO $ do
    recvLoop sock q `catch` ignoreRecvExceptions
    atomically $ writeTQueue q AppMsgDisconnect
  where
    ignoreRecvExceptions :: SomeException -> IO ()
    ignoreRecvExceptions _ = close sock

recvLoop :: Socket -> TQueue AppMsg -> IO ()
recvLoop sock q = go B.empty
  where
    go :: ByteString -> IO ()
    go buf = do
      chunk <- NSB.recv sock recvChunkSize
      if B.null chunk
        then pure ()
        else do
          let buf' = buf <> chunk
          case decodeAllFrames buf' of
            Left err -> do
              close sock
              throwIO $ userError $ "P2P.Communication.recvLoop: " ++ err
            Right (msgs, leftover) -> do
              atomically $ mapM_ (writeTQueue q) msgs
              go leftover
