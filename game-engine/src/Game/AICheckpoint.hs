module Game.AICheckpoint
  ( defaultCheckpointPath,
    saveCanonicalCheckpoint,
    loadCheckpointWeights,
  )
where

import Data.List (intercalate)
import Game.AIModel (QWeights (..))
import Game.AITraining (TrainingRunSummary (..))
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

defaultCheckpointPath :: FilePath
defaultCheckpointPath = "game-engine/data/ai_checkpoint.txt"

saveCanonicalCheckpoint :: FilePath -> TrainingRunSummary -> IO ()
saveCanonicalCheckpoint path summary =
  writeFile path (encodeCheckpoint summary)

loadCheckpointWeights :: FilePath -> IO (Maybe QWeights)
loadCheckpointWeights path = do
  contentOrEmpty <- catchIOError (readFile path) (\_ -> pure "")
  pure (decodeWeights contentOrEmpty)

encodeCheckpoint :: TrainingRunSummary -> String
encodeCheckpoint summary =
  let weights = trsCanonicalWeights summary
      coeffsLine = "coeffs=" ++ intercalate "," (map show (qCoeffs weights))
   in unlines
        [ "selected_epoch=" ++ show (trsCanonicalEpoch summary),
          "selection_score=" ++ show (trsCanonicalScore summary),
          "bias=" ++ show (qBias weights),
          coeffsLine
        ]

decodeWeights :: String -> Maybe QWeights
decodeWeights raw = do
  biasStr <- lookupKey "bias" pairs
  coeffsStr <- lookupKey "coeffs" pairs
  parsedBias <- readMaybe biasStr
  parsedCoeffs <- mapM readMaybe (splitByComma coeffsStr)
  pure QWeights {qBias = parsedBias, qCoeffs = parsedCoeffs}
  where
    pairs = map parseLine (lines raw)

parseLine :: String -> (String, String)
parseLine ln =
  let (k, rest) = break (== '=') ln
      v = case rest of
        [] -> ""
        (_ : xs) -> xs
   in (k, v)

lookupKey :: String -> [(String, String)] -> Maybe String
lookupKey key kvs = lookup key kvs

splitByComma :: String -> [String]
splitByComma input = case break (== ',') input of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitByComma rest
