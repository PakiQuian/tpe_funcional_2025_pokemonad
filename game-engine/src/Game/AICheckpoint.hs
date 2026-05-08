module Game.AICheckpoint
  ( AICheckpointData (..),
    defaultCheckpointPath,
    saveCanonicalCheckpoint,
    saveCheckpointData,
    loadCheckpointData,
    loadCheckpointWeights,
  )
where

import Data.List (intercalate)
import Control.Exception (evaluate)
import Game.AIModel (QWeights (..))
import Game.AITraining (TrainingRunSummary (..))
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

data AICheckpointData = AICheckpointData
  { acdWeights :: QWeights,
    acdTotalEpochs :: Int,
    acdBestScore :: Float
  }
  deriving (Show, Eq)

defaultCheckpointPath :: FilePath
defaultCheckpointPath = "game-engine/data/ai_checkpoint.txt"

saveCanonicalCheckpoint :: FilePath -> TrainingRunSummary -> IO ()
saveCanonicalCheckpoint path summary =
  saveCheckpointData path (AICheckpointData (trsCanonicalWeights summary) 0 (trsCanonicalScore summary))

saveCheckpointData :: FilePath -> AICheckpointData -> IO ()
saveCheckpointData path checkpoint =
  writeFile path (encodeCheckpoint checkpoint)

loadCheckpointWeights :: FilePath -> IO (Maybe QWeights)
loadCheckpointWeights path = do
  maybeData <- loadCheckpointData path
  pure (acdWeights <$> maybeData)

loadCheckpointData :: FilePath -> IO (Maybe AICheckpointData)
loadCheckpointData path = do
  contentOrEmpty <- catchIOError (readFileStrict path) (\_ -> pure "")
  pure (decodeCheckpoint contentOrEmpty)

readFileStrict :: FilePath -> IO String
readFileStrict path =
  withFile path ReadMode $ \h -> do
    contents <- hGetContents h
    _ <- evaluate (length contents)
    pure contents

encodeCheckpoint :: AICheckpointData -> String
encodeCheckpoint checkpoint =
  let weights = acdWeights checkpoint
      coeffsLine = "coeffs=" ++ intercalate "," (map show (qCoeffs weights))
   in unlines
        [ "total_epochs=" ++ show (acdTotalEpochs checkpoint),
          "best_score=" ++ show (acdBestScore checkpoint),
          "bias=" ++ show (qBias weights),
          coeffsLine
        ]

decodeCheckpoint :: String -> Maybe AICheckpointData
decodeCheckpoint raw = do
  biasStr <- lookupKey "bias" pairs
  coeffsStr <- lookupKey "coeffs" pairs
  totalEpochs <- maybe (Just 0) readMaybe (lookupKey "total_epochs" pairs)
  bestScore <- maybe (Just (-1.0e30)) readMaybe (lookupKey "best_score" pairs)
  parsedBias <- readMaybe biasStr
  parsedCoeffs <- mapM readMaybe (splitByComma coeffsStr)
  pure
    AICheckpointData
      { acdWeights = QWeights {qBias = parsedBias, qCoeffs = parsedCoeffs},
        acdTotalEpochs = totalEpochs,
        acdBestScore = bestScore
      }
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
