module Screens.MenuScreen (drawMenuScreen) where

import Graphics.Gloss

-- Las opciones del menú en texto
menuOptions :: [String]
menuOptions = 
    [ "1. Ver Pokedex"
    , "2. Conectar P2P"
    , "3. Jugar vs AI"
    ]

-- Dibuja la pantalla del menú principal
drawMenuScreen :: Picture -> Int -> Picture
drawMenuScreen menuBgImage selection = pictures 
    [ menuBgImage       -- 1. Dibujamos el fondo del menú
    , drawMenu selection -- 2. Dibujamos el menú encima
    ]

-- Función auxiliar para dibujar el menú centrado
drawMenu :: Int -> Picture
drawMenu selection = pictures (title : options)
  where
    -- Título del juego
    title = translate (-200) 200 
          $ scale 0.5 0.5 
          $ color yellow 
          $ text "POKEMONAD HASKELL"

    -- Generar la lista de opciones visuales
    options = zipWith (drawOption selection) [0..] menuOptions

-- Dibuja una opción individual. Si está seleccionada, la pone roja y grande.
drawOption :: Int -> Int -> String -> Picture
drawOption currentSelection index label = 
    let 
        isSelected = currentSelection == index
        yPosition  = fromIntegral (50 - (index * 60)) -- Separación vertical
        col        = if isSelected then red else white
        scl        = if isSelected then 0.3 else 0.2
        -- Agregamos una "flecha" o pokebola si está seleccionado
        prefix     = if isSelected then ">> " else "" 
    in
        translate (-150) yPosition 
        $ scale scl scl 
        $ color col 
        $ text (prefix ++ label)
