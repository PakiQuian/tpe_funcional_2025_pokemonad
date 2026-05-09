module Pokemonad.AI.HyperParams
  ( RewardWeights (..),
    TrainingHyperParams (..),
    defaultTrainingHyperParams,
  )
where

data RewardWeights = RewardWeights
  { rewardDamageInflicted :: Float,
    rewardDamageReceived :: Float,
    rewardEnemyKnockout :: Float,
    rewardSelfKnockout :: Float,
    rewardWinTerminal :: Float,
    rewardLoseTerminal :: Float,
    rewardPerTurnPenalty :: Float
  }
  deriving (Show, Eq)

data TrainingHyperParams = TrainingHyperParams
  { learningRate :: Float,
    discountFactor :: Float,
    epsilonStart :: Float,
    epsilonMin :: Float,
    epsilonDecay :: Float,
    gradientClip :: Float,
    switchPenalty :: Float,
    rewardWeights :: RewardWeights
  }
  deriving (Show, Eq)

defaultTrainingHyperParams :: TrainingHyperParams
defaultTrainingHyperParams =
  TrainingHyperParams
    { learningRate = 0.01,
      discountFactor = 0.95,
      epsilonStart = 1.0,
      epsilonMin = 0.05,
      epsilonDecay = 0.995,
      gradientClip = 1.0,
      switchPenalty = 0.05,
      rewardWeights =
        RewardWeights
          { rewardDamageInflicted = 1.0,
            rewardDamageReceived = -1.0,
            rewardEnemyKnockout = 5.0,
            rewardSelfKnockout = -5.0,
            rewardWinTerminal = 20.0,
            rewardLoseTerminal = -20.0,
            rewardPerTurnPenalty = -0.01
          }
    }
