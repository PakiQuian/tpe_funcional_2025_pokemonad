module Pokemonad.AI.Persistence
  ( AICheckpointData (..),
    defaultCheckpointPath,
    saveCheckpointData,
    saveCanonicalCheckpoint,
    loadCheckpointData,
    loadCheckpointWeights,
  )
where

import Control.Exception (evaluate)
import Data.List (intercalate)
import Pokemonad.AI.Model (QWeights (..))
import Pokemonad.AI.Training (TrainingRunSummary (..))
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

data AICheckpointData = AICheckpointData
  { checkpointWeights :: QWeights,
    checkpointTotalEpochs :: Int,
    checkpointBestScore :: Float
  }
  deriving (Show, Eq)

defaultCheckpointPath :: FilePath
defaultCheckpointPath = "game-engine/data/ai_checkpoint.txt"

saveCanonicalCheckpoint :: FilePath -> TrainingRunSummary -> IO ()
saveCanonicalCheckpoint path summary =
  saveCheckpointData path (AICheckpointData (summaryCanonicalWeights summary) 0 (summaryCanonicalScore summary))

saveCheckpointData :: FilePath -> AICheckpointData -> IO ()
saveCheckpointData path checkpoint =
  writeFile path (encodeCheckpoint checkpoint)

loadCheckpointWeights :: FilePath -> IO (Maybe QWeights)
loadCheckpointWeights path = do
  maybeData <- loadCheckpointData path
  pure (checkpointWeights <$> maybeData)

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
  let weights = checkpointWeights checkpoint
      coeffsLine = "coeffs=" ++ intercalate "," (map show (weightsCoefficients weights))
   in unlines
        [ "total_epochs=" ++ show (checkpointTotalEpochs checkpoint),
          "best_score=" ++ show (checkpointBestScore checkpoint),
          "bias=" ++ show (weightsBias weights),
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
      { checkpointWeights = QWeights {weightsBias = parsedBias, weightsCoefficients = parsedCoeffs},
        checkpointTotalEpochs = totalEpochs,
        checkpointBestScore = bestScore
      }
  where
    pairs = map parseLine (lines raw)

parseLine :: String -> (String, String)
parseLine ln =
  let (k, rest) = break (== '=') ln
   in (k, drop 1 rest)

lookupKey :: String -> [(String, String)] -> Maybe String
lookupKey = lookup

splitByComma :: String -> [String]
splitByComma input = case break (== ',') input of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitByComma rest
