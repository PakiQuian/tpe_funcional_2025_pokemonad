module Client.Handlers.PokedexHandler (handleUp, handleDown, handleEnter, handleBack) where

import Client.Types (PokedexState (..), Screen (..))
import Pokemonad.Core.Pokemon (allPokemon)
import Pokemonad.Core.Types (PokemonId (..))

handleUp :: PokedexState -> PokedexState
handleUp s = s {pokedexCursor = PokemonId (max 1 (unPokemonId (pokedexCursor s) - 1))}

handleDown :: PokedexState -> PokedexState
handleDown s = s {pokedexCursor = PokemonId (min (length allPokemon) (unPokemonId (pokedexCursor s) + 1))}

handleEnter :: PokedexState -> Maybe Screen
handleEnter _ = Just PokemonDetail

handleBack :: PokedexState -> Maybe Screen
handleBack _ = Just Menu
