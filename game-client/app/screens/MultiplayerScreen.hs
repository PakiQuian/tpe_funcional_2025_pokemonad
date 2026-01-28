module Screens.MultiplayerScreen (drawMultiplayerScreen) where

import Graphics.Gloss

-- Dibuja la pantalla de Multijugador P2P
drawMultiplayerScreen :: Picture
drawMultiplayerScreen = pictures 
    [ blank
    , color white $ text "Pantalla P2P (ESC para volver)"
    ]
