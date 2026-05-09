module P2P.Serialization
  ( maxFramedPayloadBytes,
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
import Data.Word (Word32)
import P2P.Types (AppMsg (..), Handshake (..), PlayerAction (..))

instance Binary Handshake where
  put (Handshake m v) = put m >> put v
  get = Handshake <$> get <*> get

instance Binary PlayerAction where
  put (UseMove idx) = putWord8 0 >> put idx
  put (SwitchPokemon idx) = putWord8 1 >> put idx
  get = do
    tag <- getWord8
    case tag of
      0 -> UseMove <$> get
      1 -> SwitchPokemon <$> get
      _ -> fail "P2P.Serialization: unknown PlayerAction tag"

instance Binary AppMsg where
  put msg = case msg of
    AppMsgHandshake h -> putWord8 0 >> put h
    AppMsgTeam ids -> putWord8 1 >> put ids
    AppMsgBattleReady -> putWord8 2
    AppMsgAction action -> putWord8 3 >> put action
    AppMsgBattleState bs -> putWord8 4 >> put bs
    AppMsgDisconnect -> putWord8 5
  get = do
    tag <- getWord8
    case tag of
      0 -> AppMsgHandshake <$> get
      1 -> AppMsgTeam <$> get
      2 -> pure AppMsgBattleReady
      3 -> AppMsgAction <$> get
      4 -> AppMsgBattleState <$> get
      5 -> pure AppMsgDisconnect
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

decodeAllFrames :: ByteString -> Either String ([AppMsg], ByteString)
decodeAllFrames = go []
  where
    go acc bs = case decodeOneFrame bs of
      FrameNeedMore leftover -> Right (reverse acc, leftover)
      FrameError e -> Left e
      FrameOk msg rest -> go (msg : acc) rest
