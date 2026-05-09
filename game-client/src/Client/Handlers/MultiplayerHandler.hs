module Client.Handlers.MultiplayerHandler
  ( handleMultiplayerChar,
    handleMultiplayerEnter,
    startMultiplayerNet,
  )
where

import Client.State (GameState (..), World (..))
import Client.Types (MultiplayerIntent (..), NetConnAsync (..), NetSubState (..))
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

handleMultiplayerChar :: Char -> GameState -> GameState
handleMultiplayerChar c state =
  case multiplayerRow state of
    0 ->
      if isHostChar c
        then state {multiplayerHost = multiplayerHost state ++ [c], multiplayerError = Nothing}
        else state
    1 ->
      if isDigit c
        then state {multiplayerPort = multiplayerPort state ++ [c], multiplayerError = Nothing}
        else state
    _ -> state

handleMultiplayerEnter :: GameState -> GameState
handleMultiplayerEnter state =
  case multiplayerRow state of
    0 -> state {multiplayerRow = 1}
    1 -> state {multiplayerRow = 2}
    2 ->
      case parsePortStr (multiplayerPort state) of
        Nothing -> state {multiplayerError = Just "Invalid port (1-65535)."}
        Just p -> state {multiplayerPending = Just (MPListen p), multiplayerError = Nothing}
    3 ->
      case parsePortStr (multiplayerPort state) of
        Nothing -> state {multiplayerError = Just "Invalid port (1-65535)."}
        Just p ->
          let h = multiplayerHost state
           in if null h
                then state {multiplayerError = Just "Please enter a host."}
                else state {multiplayerPending = Just (MPConnect h p), multiplayerError = Nothing}
    _ -> state

runListen :: PortNumber -> World -> IO ()
runListen port w = do
  r <- try (listenAndAccept port) :: IO (Either SomeException (Socket, SockAddr))
  case r of
    Left e -> atomically $ writeTVar (netConnAsync w) (Just $ NetConnErr (show e))
    Right (sock, _) -> do
      _ <- forkRecvLoop sock (netInQueue w)
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnOk sock NetInLobby)

runConnect :: HostName -> PortNumber -> World -> IO ()
runConnect host port w = do
  r <- try (connectTo host port) :: IO (Either SomeException Socket)
  case r of
    Left e -> atomically $ writeTVar (netConnAsync w) (Just $ NetConnErr (show e))
    Right sock -> do
      _ <- forkRecvLoop sock (netInQueue w)
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnOk sock NetInLobby)

startMultiplayerNet :: MultiplayerIntent -> World -> IO World
startMultiplayerNet intent w = case netSubState w of
  NetListening _ ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "Already listening."}}
  NetConnecting _ _ ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "Connection in progress."}}
  NetInLobby ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "Already connected."}}
  NetInBattle ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "In battle."}}
  NetDisconnected ->
    case intent of
      MPListen port -> do
        void $ forkIO (runListen port w)
        pure
          w
            { netSubState = NetListening (fromIntegral port),
              worldGame = (worldGame w) {multiplayerError = Nothing}
            }
      MPConnect host port -> do
        void $ forkIO (runConnect host port w)
        pure
          w
            { netSubState = NetConnecting host (fromIntegral port),
              worldGame = (worldGame w) {multiplayerError = Nothing}
            }
