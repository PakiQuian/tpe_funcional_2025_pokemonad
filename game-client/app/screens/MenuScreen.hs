module Screens.MenuScreen (drawMenuScreen) where

import Graphics.Gloss
import Engine.Common (pokemonBlue, pokemonYellow, drawLogo)

-- Las opciones del menú
menuOptions :: [String]
menuOptions = 
    [ "POKEDEX"
    , "P2P CONNECT"
    , "SINGLE PLAYER"
    ]

-- Dibuja la pantalla completa
-- Recibe: Fondo (Picture) -> Logo (Picture) -> Selección (Int) -> Resultado
drawMenuScreen :: Picture -> Picture -> Int -> Picture
drawMenuScreen menuBgImage logoImage selection = pictures 
    [ menuBgImage             -- 1. Fondo (Wallpaper)
    , drawLogo logoImage      -- 2. Logo del juego
    , drawMenuBox selection   -- 3. Caja del menú y opciones
    ]

-- Contenedor principal del menú
drawMenuBox :: Int -> Picture
drawMenuBox selection = translate 0 (-100) $ pictures
    [ -- Caja de fondo del menú
      color white       $ rectangleSolid 520 270       -- Borde Blanco
    , color pokemonBlue $ rectangleSolid 500 250       -- Fondo Azul
    , translate (-200) 50 $ pictures (zipWith (drawOption selection) [0..] menuOptions)
    ]

-- Dibuja una sola opción (Texto + Cursor)
drawOption :: Int -> Int -> String -> Picture
drawOption currentSelection index label = 
    let 
        isSelected = currentSelection == index
        yPos = fromIntegral index * (-60)
        
        -- Estilo del texto
        txtColor = if isSelected then white else makeColorI 180 180 180 255
        txtScale = 0.25
        
        -- El texto renderizado
        txtItem = translate 40 yPos 
                $ scale txtScale txtScale 
                $ color txtColor 
                $ text label
        
        -- El cursor (triángulo ►) clásico de RPG
        cursor = if isSelected 
                 then translate 10 (yPos + 3) 
                      $ color pokemonYellow 
                      $ polygon [(0,0), (0, 20), (15, 10)] -- Triángulo
                 else blank
    in
        pictures [cursor, txtItem]