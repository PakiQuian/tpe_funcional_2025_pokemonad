module Client.Handlers.MultiplayerHandler
  ( handleChar,
    handleEnter,
    handleBack,
    startMultiplayerNet,
  )
where

import Client.State (AppState (..), World (..))
import Client.Types
  ( MultiplayerIntent (..),
    MultiplayerState (..),
    NetConnAsync (..),
    NetSubState (..),
    Screen (..),
    defaultMultiplayerState,
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Network.Socket (HostName, PortNumber, SockAddr, Socket)
import P2P.Communication (connectTo, forkRecvLoop, listenAndAccept)
import Text.Read (readMaybe)

parsePortStr :: String -> Maybe PortNumber
parsePortStr s
  | null s = Nothing
  | otherwise = case readMaybe s :: Maybe Int of
      Just n | n >= 1 && n <= 65535 -> Just (fromIntegral n :: PortNumber)
      _ -> Nothing

isHostChar :: Char -> Bool
isHostChar c = isAlphaNum c || c `elem` (".-_" :: String)

-- ---------------------------------------------------------------------------
-- Pure handlers
-- ---------------------------------------------------------------------------

handleChar :: Char -> MultiplayerState -> MultiplayerState
handleChar c ms =
  case mpCursor ms of
    0 ->
      if isHostChar c
        then ms {mpHost = mpHost ms ++ [c], mpError = Nothing}
        else ms
    1 ->
      if isDigit c
        then ms {mpPort = mpPort ms ++ [c], mpError = Nothing}
        else ms
    _ -> ms

handleEnter :: MultiplayerState -> (MultiplayerState, Maybe Screen)
handleEnter ms =
  case mpCursor ms of
    0 -> (ms {mpCursor = 1}, Nothing)
    1 -> (ms {mpCursor = 2}, Nothing)
    2 ->
      case parsePortStr (mpPort ms) of
        Nothing -> (ms {mpError = Just "Invalid port (1-65535)."}, Nothing)
        Just p -> (ms {mpPending = Just (MPListen p), mpError = Nothing}, Nothing)
    3 ->
      case parsePortStr (mpPort ms) of
        Nothing -> (ms {mpError = Just "Invalid port (1-65535)."}, Nothing)
        Just p ->
          let h = mpHost ms
           in if null h
                then (ms {mpError = Just "Please enter a host."}, Nothing)
                else (ms {mpPending = Just (MPConnect h p), mpError = Nothing}, Nothing)
    4 -> (ms, Just TeamSelect)
    _ -> (ms, Nothing)

handleBack :: MultiplayerState -> (MultiplayerState, Maybe Screen)
handleBack ms =
  case mpCursor ms of
    0 ->
      if null (mpHost ms)
        then (defaultMultiplayerState, Just Menu)
        else (ms {mpHost = init (mpHost ms)}, Nothing)
    1 ->
      if null (mpPort ms)
        then (ms {mpCursor = 0}, Nothing)
        else (ms {mpPort = init (mpPort ms)}, Nothing)
    _ -> (defaultMultiplayerState, Just Menu)

-- ---------------------------------------------------------------------------
-- IO networking
-- ---------------------------------------------------------------------------

runListen :: PortNumber -> World -> IO ()
runListen port w = do
  r <- try (listenAndAccept port) :: IO (Either SomeException (Socket, SockAddr))
  case r of
    Left e -> atomically $ writeTVar (netConnAsync w) (Just $ NetConnErr (show e))
    Right (sock, _) -> do
      _ <- forkRecvLoop sock (netInQueue w)
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnOk sock NetInLobby True)

runConnect :: HostName -> PortNumber -> World -> IO ()
runConnect host port w = do
  r <- try (connectTo host port) :: IO (Either SomeException Socket)
  case r of
    Left e -> atomically $ writeTVar (netConnAsync w) (Just $ NetConnErr (show e))
    Right sock -> do
      _ <- forkRecvLoop sock (netInQueue w)
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnOk sock NetInLobby False)

startMultiplayerNet :: MultiplayerIntent -> World -> IO World
startMultiplayerNet intent w =
  let setErr msg = w {worldGame = (worldGame w) {multiplayerState = (multiplayerState (worldGame w)) {mpError = Just msg}}}
      clearErr = w {worldGame = (worldGame w) {multiplayerState = (multiplayerState (worldGame w)) {mpError = Nothing}}}
   in case netSubState w of
        NetListening _ -> pure (setErr "Already listening.")
        NetConnecting _ _ -> pure (setErr "Connection in progress.")
        NetInLobby -> pure (setErr "Already connected.")
        NetInBattle -> pure (setErr "In battle.")
        NetDisconnected ->
          case intent of
            MPListen port -> do
              void $ forkIO (runListen port w)
              pure clearErr {netSubState = NetListening (fromIntegral port)}
            MPConnect host port -> do
              void $ forkIO (runConnect host port w)
              pure clearErr {netSubState = NetConnecting host (fromIntegral port)}
