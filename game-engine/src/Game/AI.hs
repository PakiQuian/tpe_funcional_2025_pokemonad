module Game.AI (chooseEnemyAction) where

import Game.Battle (BattleAction (..), BattleState)
import Game.Trainer (AIDifficulty)
import System.Random (StdGen)

chooseEnemyAction :: StdGen -> AIDifficulty -> BattleState -> (BattleAction, StdGen)
chooseEnemyAction rng _difficulty _battleState =
  -- TODO: Ajustar precision y calidad de decisión en funcion de la dificultad recibida.
  -- TODO: Implementar estrategia IA real en base al estado de batalla (sin conocer la accion del jugador).
  (ActionMove 0, rng)