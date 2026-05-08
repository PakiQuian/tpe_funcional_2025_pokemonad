module Game.AIHyperParams
  ( TrainingHyperParams (..),
    RewardWeights (..),
    defaultTrainingHyperParams,
  )
where

-- | Reward shaping coefficients for TD training.
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

-- | Hardcoded hyperparameters for offline self-play training.
--   Contract: these are static constants in code (no runtime tuning UI).
data TrainingHyperParams = TrainingHyperParams
  { hpLearningRateAlpha :: Float,
    hpDiscountGamma :: Float,
    hpEpsilonStart :: Float,
    hpEpsilonMin :: Float,
    hpEpsilonDecay :: Float,
    hpGradientClip :: Float,
    hpSwitchPenalty :: Float,
    hpRewardWeights :: RewardWeights
  }
  deriving (Show, Eq)

defaultTrainingHyperParams :: TrainingHyperParams
defaultTrainingHyperParams =
  TrainingHyperParams
    { hpLearningRateAlpha = 0.01,
      hpDiscountGamma = 0.95,
      hpEpsilonStart = 1.0,
      hpEpsilonMin = 0.05,
      hpEpsilonDecay = 0.995,
      hpGradientClip = 1.0,
      hpSwitchPenalty = 0.05,
      hpRewardWeights =
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
