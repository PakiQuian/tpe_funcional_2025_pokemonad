module Client.Handlers.TeamSelectHandler
  ( handleTeamSelectEnter,
    handleRandomTeam,
    addPokemonToTeam,
  )
where

import Client.State (GameState (..))
import Client.Types (Screen (..))
import Pokemonad.Core.Pokemon (allPokemon)
import Pokemonad.Core.Types (PokemonId (..))
import System.Random (StdGen, randomR)

handleTeamSelectEnter :: GameState -> GameState
handleTeamSelectEnter state
  | length (playerTeam state) == 6 = state {currentScreen = OpponentSelect}
  | otherwise = addPokemonToTeam state

addPokemonToTeam :: GameState -> GameState
addPokemonToTeam state
  | pid `elem` playerTeam state = state
  | length (playerTeam state) >= 6 = state
  | otherwise = state {playerTeam = playerTeam state ++ [pid]}
  where
    pid = selectedPokemonId state

handleRandomTeam :: GameState -> GameState
handleRandomTeam state =
  let maxId = length allPokemon
      (newTeam, nextGen) = generateUniqueRandoms 6 maxId [] (randomGen state)
   in state {playerTeam = newTeam, randomGen = nextGen}

generateUniqueRandoms :: Int -> Int -> [PokemonId] -> StdGen -> ([PokemonId], StdGen)
generateUniqueRandoms 0 _ acc gen = (acc, gen)
generateUniqueRandoms n maxIdx acc gen =
  let (r, nextGen) = randomR (1, maxIdx) gen
      pid = PokemonId r
   in if pid `elem` acc
        then generateUniqueRandoms n maxIdx acc nextGen
        else generateUniqueRandoms (n - 1) maxIdx (pid : acc) nextGen
