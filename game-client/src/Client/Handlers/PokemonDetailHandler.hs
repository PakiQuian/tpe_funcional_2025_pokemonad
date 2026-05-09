module Client.Handlers.PokemonDetailHandler (handleBack) where

import Client.Types (Screen (..))

handleBack :: Maybe Screen
handleBack = Just Pokedex
