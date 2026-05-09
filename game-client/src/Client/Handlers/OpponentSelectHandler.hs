module Client.Handlers.OpponentSelectHandler (handleOpponentSelectEnter) where

import Client.Handlers.BattleHandler (firstSwitchableBenchIndexFromBattle)
import Client.State (GameState (..))
import Client.Types (BattleMenuType (..), Screen (..))
import Pokemonad.Battle.State (initBattle)
import Pokemonad.Core.Trainer (allTrainers)
import System.Random (randomR)

handleOpponentSelectEnter :: GameState -> GameState
handleOpponentSelectEnter state =
  let trainer = allTrainers !! selectedTrainerIndex state
      newBattle = initBattle (playerTeam state) trainer
      bgCount = length (battleBackgrounds state)
      (randIndex, nextRng) =
        if bgCount > 0
          then randomR (0, bgCount - 1) (randomGen state)
          else (0, randomGen state)
   in state
        { currentScreen = BattleScreen,
          selectedTrainer = Just trainer,
          battleState = Just newBattle,
          currentBattleBg = randIndex,
          randomGen = nextRng,
          battleMenuType = MainBattleMenu,
          battleBenchIndex = firstSwitchableBenchIndexFromBattle newBattle
        }
