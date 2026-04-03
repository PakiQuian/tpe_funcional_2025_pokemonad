module P2P.Serialization
  ( -- handshake / versión
    protocolMagic,
    currentProtocolVersion,
    -- tipos de mensaje
    Handshake (..),
    AppMsg (..),
    -- enmarcar y parsear streams
    maxFramedPayloadBytes,
    encodeFramed,
    FrameParseResult (..),
    decodeOneFrame,
    decodeAllFrames,
  )
where

import Data.Binary (Binary (..), decodeOrFail, encode)
import Data.Binary.Get (getWord32be, getWord8, runGetOrFail)
import Data.Binary.Put (putLazyByteString, putWord32be, putWord8, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word16, Word32)

-- Los dos lados deben coincidir en handshake (si no, peer equivocado / protocolo distinto).
protocolMagic :: Word32
protocolMagic = 0x504B4D01 -- "PKM" + 0x01

currentProtocolVersion :: Word16
currentProtocolVersion = 1 -- subir si cambia el wire format de AppMsg

data Handshake = Handshake
  { handshakeMagic :: Word32,
    handshakeVersion :: Word16
  }
  deriving (Eq, Show)

instance Binary Handshake where
  put (Handshake m v) = put m >> put v
  get = Handshake <$> get <*> get

data AppMsg
  = AppMsgHandshake Handshake
  | AppMsgTeam [Word32]
  | AppMsgBattleReady
  | AppMsgTurnStub Word32
  deriving (Eq, Show)

instance Binary AppMsg where
  put msg = case msg of
    AppMsgHandshake h -> putWord8 0 >> put h
    AppMsgTeam ids -> putWord8 1 >> put ids
    AppMsgBattleReady -> putWord8 2
    AppMsgTurnStub n -> putWord8 3 >> put n

  get = do
    tag <- getWord8
    case tag of
      0 -> AppMsgHandshake <$> get
      1 -> AppMsgTeam <$> get
      2 -> pure AppMsgBattleReady
      3 -> AppMsgTurnStub <$> get
      _ -> fail "P2P.Serialization: unknown AppMsg tag"

maxFramedPayloadBytes :: Int
maxFramedPayloadBytes = 16 * 1024 * 1024

encodeFramed :: AppMsg -> ByteString
encodeFramed msg =
  let payload = encode msg
      n64 = BL.length payload
   in if n64 > fromIntegral maxFramedPayloadBytes || n64 > fromIntegral (maxBound :: Word32)
        then error "P2P.Serialization.encodeFramed: payload too large"
        else
          let n = fromIntegral n64 :: Word32
           in BL.toStrict $ runPut (putWord32be n >> putLazyByteString payload)

data FrameParseResult
  = FrameNeedMore ByteString
  | FrameOk AppMsg ByteString
  | FrameError String

decodeOneFrame :: ByteString -> FrameParseResult
decodeOneFrame buf
  | B.length buf < 4 = FrameNeedMore buf
  | otherwise =
      case runGetOrFail getWord32be (BL.fromStrict (B.take 4 buf)) of
        Left (_, _, e) -> FrameError e
        Right (_, _, lenW) ->
          let len = fromIntegral lenW :: Int
           in if len > maxFramedPayloadBytes
                then FrameError "frame length out of allowed range"
                else
                  if B.length buf < 4 + len
                    then FrameNeedMore buf
                    else
                      let body = B.take len (B.drop 4 buf)
                          rest = B.drop (4 + len) buf
                       in case decodeOrFail (BL.fromStrict body) of
                            Left (_, _, e) -> FrameError e
                            Right (_, _, msg) -> FrameOk msg rest

-- Varios mensajes seguidos en el mismo buffer; el sobrante sigue en el acumulador del socket.
decodeAllFrames :: ByteString -> Either String ([AppMsg], ByteString)
decodeAllFrames = go []
  where
    go acc bs = case decodeOneFrame bs of
      FrameNeedMore leftover -> Right (reverse acc, leftover)
      FrameError e -> Left e
      FrameOk msg rest -> go (msg : acc) rest
