module Game.AI
  ( chooseEnemyAction,
    chooseEnemyActionWithWeights,
    chooseEnemyActionWithMaybeWeights,
    defaultCheckpointPath,
    saveCanonicalCheckpoint,
    loadCheckpointWeights,
    TrainingHyperParams (..),
    RewardWeights (..),
    defaultTrainingHyperParams,
    EpochMetrics (..),
    TrainingRunSummary (..),
    checkpointSelectionScore,
    runTrainingEpochs,
    runTrainingEpochsDetailed,
    FeatureVector,
    QWeights (..),
    defaultQWeights,
    candidateActions,
    extractFeatures,
    qValue,
    chooseActionGreedy,
    chooseActionEpsilon,
    difficultyExplorationRate,
  )
where

import Game.AIModel
  ( FeatureVector,
    QWeights (..),
    candidateActions,
    chooseActionEpsilon,
    chooseActionGreedy,
    defaultQWeights,
    difficultyExplorationRate,
    extractFeatures,
    qValue,
  )
import Game.AICheckpoint
  ( defaultCheckpointPath,
    loadCheckpointWeights,
    saveCanonicalCheckpoint,
  )
import Game.AITraining
  ( EpochMetrics (..),
    TrainingRunSummary (..),
    checkpointSelectionScore,
    runTrainingEpochs,
    runTrainingEpochsDetailed,
  )
import Game.Battle (BattleAction (..), BattleState)
import Game.AIHyperParams
  ( RewardWeights (..),
    TrainingHyperParams (..),
    defaultTrainingHyperParams,
  )
import Game.Trainer (AIDifficulty)
import System.Random (StdGen)

chooseEnemyAction :: StdGen -> AIDifficulty -> BattleState -> (BattleAction, StdGen)
chooseEnemyAction rng difficulty battleState =
  chooseEnemyActionWithWeights rng difficulty defaultQWeights battleState

chooseEnemyActionWithWeights :: StdGen -> AIDifficulty -> QWeights -> BattleState -> (BattleAction, StdGen)
chooseEnemyActionWithWeights rng difficulty weights battleState =
  let epsilon = difficultyExplorationRate difficulty
      (chosenMaybe, nextRng) = chooseActionEpsilon rng epsilon weights battleState
   in (maybe (ActionMove 0) id chosenMaybe, nextRng)

chooseEnemyActionWithMaybeWeights :: StdGen -> AIDifficulty -> Maybe QWeights -> BattleState -> (BattleAction, StdGen)
chooseEnemyActionWithMaybeWeights rng difficulty maybeWeights battleState =
  chooseEnemyActionWithWeights rng difficulty (maybe defaultQWeights id maybeWeights) battleState