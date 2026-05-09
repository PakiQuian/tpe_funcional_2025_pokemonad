module Client.Handlers.TeamSelectHandler
  ( handleUp,
    handleDown,
    handleEnter,
    handleBack,
    handleRandomTeam,
  )
where

import Client.Types (Screen (..), TeamSelectState (..))
import Pokemonad.Core.Pokemon (allPokemon)
import Pokemonad.Core.Types (PokemonId (..))
import System.Random (StdGen, randomR)

handleUp :: TeamSelectState -> TeamSelectState
handleUp s = s {teamSelectCursor = PokemonId (max 1 (unPokemonId (teamSelectCursor s) - 1))}

handleDown :: TeamSelectState -> TeamSelectState
handleDown s = s {teamSelectCursor = PokemonId (min (length allPokemon) (unPokemonId (teamSelectCursor s) + 1))}

-- Returns (newState, newTeam, transition)
handleEnter :: TeamSelectState -> [PokemonId] -> (TeamSelectState, [PokemonId], Maybe Screen)
handleEnter s team
  | length team == 6 = (s, team, Just OpponentSelect)
  | pid `elem` team = (s, team, Nothing)
  | length team >= 6 = (s, team, Nothing)
  | otherwise = (s, team ++ [pid], Nothing)
  where
    pid = teamSelectCursor s

-- Returns (newState, newTeam, transition)
handleBack :: TeamSelectState -> [PokemonId] -> (TeamSelectState, [PokemonId], Maybe Screen)
handleBack s team
  | null team = (s, team, Just Menu)
  | otherwise = (s, init team, Nothing)

-- Returns (newState, newRng, newTeam)
handleRandomTeam :: TeamSelectState -> StdGen -> (TeamSelectState, StdGen, [PokemonId])
handleRandomTeam s gen =
  let (newTeam, nextGen) = generateUniqueRandoms 6 (length allPokemon) [] gen
   in (s, nextGen, newTeam)

generateUniqueRandoms :: Int -> Int -> [PokemonId] -> StdGen -> ([PokemonId], StdGen)
generateUniqueRandoms 0 _ acc gen = (acc, gen)
generateUniqueRandoms n maxIdx acc gen =
  let (r, nextGen) = randomR (1, maxIdx) gen
      pid = PokemonId r
   in if pid `elem` acc
        then generateUniqueRandoms n maxIdx acc nextGen
        else generateUniqueRandoms (n - 1) maxIdx (pid : acc) nextGen
