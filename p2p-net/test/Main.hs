-- | Prueba de la capa de serialización P2P.
--
--   Valida la ley de ida y vuelta del protocolo enmarcado: cualquier
--   secuencia de mensajes codificada con 'encodeFramed' se recupera intacta
--   con 'decodeAllFrames', sin bytes sobrantes.
module Main (main) where

import qualified Data.ByteString as BS
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (Gen, arbitrary, oneof, testProperty)
import qualified Test.Tasty.QuickCheck as QC

import P2P.Serialization (decodeAllFrames, encodeFramed)
import P2P.Types (AppMsg (..), Handshake (..), PlayerAction (..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "p2p-net"
    [ testGroup
        "Serialización enmarcada"
        [ testProperty "decodeAllFrames . concat . map encodeFramed = id" $ \msgs ->
            decodeAllFrames (BS.concat (map encodeFramed msgs)) == Right (msgs, BS.empty)
        ]
    ]

genBS :: Gen BS.ByteString
genBS = BS.pack <$> arbitrary

instance QC.Arbitrary Handshake where
  arbitrary = Handshake <$> arbitrary <*> arbitrary

instance QC.Arbitrary PlayerAction where
  arbitrary = oneof [UseMove <$> arbitrary, SwitchPokemon <$> arbitrary]

instance QC.Arbitrary AppMsg where
  arbitrary =
    oneof
      [ AppMsgHandshake <$> arbitrary,
        AppMsgTeam <$> arbitrary,
        pure AppMsgBattleReady,
        AppMsgAction <$> arbitrary,
        AppMsgBattleState <$> genBS,
        AppMsgBattleFrames <$> genBS,
        pure AppMsgDisconnect
      ]
