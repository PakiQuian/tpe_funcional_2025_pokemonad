module Screens.AIScreen (drawAIScreen) where

import Graphics.Gloss

-- Dibuja la pantalla de juego contra AI
drawAIScreen :: Picture
drawAIScreen = pictures 
    [ blank
    , color white $ text "Pantalla AI (ESC para volver)"
    ]
