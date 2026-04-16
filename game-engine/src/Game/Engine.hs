module Game.Engine
  ( BattleAction (..),
    BattlePhase (..),
    BattleState (..),
    Winner (..),
    initBattleEngine,
    submitPlayerAction,
  )
where

import Game.AI (chooseEnemyAction)
import Game.Battle
  ( BattleAction (..),
    BattlePhase (..),
    BattleState (..),
    Winner (..),
    enemyDifficulty,
    executeTurn,
    initBattle,
  )
import Game.Trainer (Trainer)
import System.Random (StdGen)

initBattleEngine :: [Int] -> Trainer -> BattleState
initBattleEngine = initBattle

submitPlayerAction :: StdGen -> BattleState -> BattleAction -> (BattleState, StdGen)
submitPlayerAction rng bState playerAction =
  let (enemyAction, rngAfterAI) = chooseEnemyAction rng (enemyDifficulty bState) bState
      nextBattleState = executeTurn bState playerAction enemyAction
   in (nextBattleState, rngAfterAI)
