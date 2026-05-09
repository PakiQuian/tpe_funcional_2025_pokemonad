module Pokemonad.AI.Decision
  ( QWeights,
    chooseEnemyAction,
    chooseEnemyActionWithWeights,
    chooseEnemyActionWithMaybeWeights,
  )
where

import Pokemonad.AI.Model
  ( QWeights (..),
    chooseActionEpsilon,
    defaultQWeights,
    difficultyExplorationRate,
  )
import Pokemonad.Battle.State (BattleAction (..), BattleState)
import Pokemonad.Core.Trainer (AIDifficulty)
import System.Random (StdGen)

chooseEnemyAction :: StdGen -> AIDifficulty -> BattleState -> (BattleAction, StdGen)
chooseEnemyAction rng difficulty bState =
  chooseEnemyActionWithWeights rng difficulty defaultQWeights bState

chooseEnemyActionWithWeights :: StdGen -> AIDifficulty -> QWeights -> BattleState -> (BattleAction, StdGen)
chooseEnemyActionWithWeights rng difficulty weights bState =
  let epsilon = difficultyExplorationRate difficulty
      (chosen, nextRng) = chooseActionEpsilon rng epsilon weights bState
   in (maybe (ActionMove 0) id chosen, nextRng)

chooseEnemyActionWithMaybeWeights :: StdGen -> AIDifficulty -> Maybe QWeights -> BattleState -> (BattleAction, StdGen)
chooseEnemyActionWithMaybeWeights rng difficulty maybeWeights bState =
  chooseEnemyActionWithWeights rng difficulty (maybe defaultQWeights id maybeWeights) bState
