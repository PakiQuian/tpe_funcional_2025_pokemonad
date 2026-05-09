module Client.Handlers.OpponentSelectHandler
  ( handleUp,
    handleDown,
    handleEnter,
  )
where

import Client.Handlers.BattleHandler (firstSwitchableBenchIndexFromBattle)
import Client.Types
  ( BattleMenuType (..),
    BattleScreenState (..),
    OpponentSelectState (..),
    Screen (..),
    defaultBattleScreenState,
  )
import Pokemonad.Battle.State (initBattle)
import Pokemonad.Core.Trainer (Trainer, allTrainers)
import Pokemonad.Core.Types (PokemonId)
import System.Random (StdGen, randomR)

handleUp :: OpponentSelectState -> OpponentSelectState
handleUp s = s {trainerCursor = max 0 (trainerCursor s - 1)}

handleDown :: OpponentSelectState -> OpponentSelectState
handleDown s = s {trainerCursor = min (length allTrainers - 1) (trainerCursor s + 1)}

-- Returns (newState, trainer, newBattleScreenState, newRng, transition)
handleEnter ::
  OpponentSelectState ->
  [PokemonId] ->
  Int ->
  StdGen ->
  (OpponentSelectState, Trainer, BattleScreenState, StdGen, Maybe Screen)
handleEnter s team bgCount gen =
  let trainer = allTrainers !! trainerCursor s
      newBattle = initBattle team trainer
      (randIdx, nextRng) =
        if bgCount > 0 then randomR (0, bgCount - 1) gen else (0, gen)
      bss =
        defaultBattleScreenState
          { currentBattle = Just newBattle,
            battleBgIndex = randIdx,
            battleBenchCursor = firstSwitchableBenchIndexFromBattle newBattle
          }
   in (s, trainer, bss, nextRng, Just BattleScreen)
