module Client.Handlers.AISimulatorHandler
  ( launchAITrainingIfRequested,
    mergeAITraining,
    applyAIResult,
  )
where

import Client.State (GameState (..), World (..))
import Client.Types (AITrainingResult (..), Screen (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad (void)
import Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..), KeyState (..), SpecialKey (..))
import Pokemonad.AI.HyperParams (defaultTrainingHyperParams)
import Pokemonad.AI.Model (defaultQWeights)
import Pokemonad.AI.Persistence
  ( AICheckpointData (..),
    defaultCheckpointPath,
    loadCheckpointData,
    saveCheckpointData,
  )
import Pokemonad.AI.Training (EpochMetrics (..), TrainingRunSummary (..), runTrainingEpochsDetailed)

launchAITrainingIfRequested :: Event -> World -> IO World
launchAITrainingIfRequested ev w
  | currentScreen gs /= AISimulator = pure w
  | not isEnterDown = pure w
  | simulatorTraining gs = pure w
  | otherwise = do
      let rng = randomGen gs
      void $ forkIO $ do
        maybeCheckpoint <- loadCheckpointData defaultCheckpointPath
        let priorEpochs = maybe 0 checkpointTotalEpochs maybeCheckpoint
            priorBestScore = maybe (-1.0e30) checkpointBestScore maybeCheckpoint
            priorWeights = maybe defaultQWeights checkpointWeights maybeCheckpoint
            epochsToRun = 100
            (summary, nextRng) = runTrainingEpochsDetailed rng defaultTrainingHyperParams epochsToRun
            newTotal = priorEpochs + epochsToRun
            newScore = summaryCanonicalScore summary
            (finalWeights, finalScore) =
              if newScore > priorBestScore
                then (summaryCanonicalWeights summary, newScore)
                else (priorWeights, priorBestScore)
            newLogs = reverse (map (formatEpochLine priorEpochs) (summaryMetrics summary))
            statusMsg =
              if newScore > priorBestScore
                then "New best! Score: " ++ shortFloat newScore ++ " | Total epochs: " ++ show newTotal
                else "No improvement. Best score: " ++ shortFloat finalScore ++ " | Total epochs: " ++ show newTotal
            nextCheckpoint =
              AICheckpointData
                { checkpointWeights = finalWeights,
                  checkpointTotalEpochs = newTotal,
                  checkpointBestScore = finalScore
                }
        saveCheckpointData defaultCheckpointPath nextCheckpoint
        atomically $
          writeTVar (aiTrainingAsync w) $
            Just
              AITrainingResult
                { atrWeights = finalWeights,
                  atrTotalEpochs = newTotal,
                  atrLogs = newLogs,
                  atrStatus = statusMsg,
                  atrRng = nextRng
                }
      pure
        w
          { worldGame =
              gs
                { simulatorTraining = True,
                  simulatorStatus = "Training in progress..."
                }
          }
  where
    gs = worldGame w
    isEnterDown = case ev of
      EventKey (SpecialKey KeyEnter) Down _ _ -> True
      _ -> False

mergeAITraining :: World -> IO World
mergeAITraining w = do
  m <- atomically $ do
    x <- readTVar (aiTrainingAsync w)
    writeTVar (aiTrainingAsync w) Nothing
    pure x
  case m of
    Nothing -> pure w
    Just result -> pure w {worldGame = applyAIResult result (worldGame w)}

applyAIResult :: AITrainingResult -> GameState -> GameState
applyAIResult result gs =
  gs
    { randomGen = atrRng result,
      enemyAIWeights = Just (atrWeights result),
      simulatorTotalEpochs = atrTotalEpochs result,
      simulatorStatus = atrStatus result,
      simulatorLogs = atrLogs result ++ simulatorLogs gs,
      simulatorTraining = False
    }

formatEpochLine :: Int -> EpochMetrics -> String
formatEpochLine epochOffset m =
  "E"
    ++ show (epochOffset + epochIndex m)
    ++ " eps="
    ++ shortFloat (epochEpsilon m)
    ++ " reward="
    ++ shortFloat (epochAverageReward m)
    ++ " win="
    ++ shortFloat (epochWinRate m)
    ++ " turns="
    ++ shortFloat (epochAverageTurns m)

shortFloat :: Float -> String
shortFloat x =
  let scaled = fromIntegral (round (x * 100.0) :: Int) / 100.0 :: Float
   in show scaled
