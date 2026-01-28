module Screens.PokedexScreen (drawPokedexScreen) where

import Graphics.Gloss

-- Dibuja la pantalla de Pokedex
drawPokedexScreen :: Picture
drawPokedexScreen = pictures 
    [ blank
    , color white $ text "Pantalla Pokedex (ESC para volver)"
    ]
