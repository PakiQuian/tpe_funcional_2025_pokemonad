module P2P.Types
  ( protocolMagic,
    currentProtocolVersion,
    Handshake (..),
    PlayerAction (..),
    AppMsg (..),
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word16, Word32)

protocolMagic :: Word32
protocolMagic = 0x504B4D01

currentProtocolVersion :: Word16
currentProtocolVersion = 1

data Handshake = Handshake
  { handshakeMagic :: Word32,
    handshakeVersion :: Word16
  }
  deriving (Eq, Show)

data PlayerAction
  = UseMove Int
  | SwitchPokemon Int
  deriving (Eq, Show)

data AppMsg
  = AppMsgHandshake Handshake
  | AppMsgTeam [Word32]
  | AppMsgBattleReady
  | AppMsgAction PlayerAction
  | AppMsgBattleState ByteString
  | AppMsgBattleFrames ByteString
  | AppMsgDisconnect
  deriving (Eq, Show)
