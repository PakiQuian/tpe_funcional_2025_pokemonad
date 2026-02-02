module Screens.PokemonScreen (drawPokemonScreen) where

import Graphics.Gloss
import Engine.Common (pokemonBlue, pokemonYellow, drawLogo)

-- Dibuja la pantalla de detalle de un Pokemon individual
-- Recibe: Fondo (Picture) -> Logo (Picture) -> Pokemon ID (Int) -> Resultado
drawPokemonScreen :: Picture -> Picture -> Int -> Picture
drawPokemonScreen menuBgImage logoImage pokemonId = pictures 
    [ menuBgImage                           -- 1. Fondo (Wallpaper)
    , drawLogo logoImage                    -- 2. Logo del juego
    , drawPokemonDetailBox pokemonId        -- 3. Caja con detalles del Pokemon
    , drawInstructions                      -- 4. Instrucciones
    ]

-- Contenedor principal del detalle del Pokemon
drawPokemonDetailBox :: Int -> Picture
drawPokemonDetailBox pokemonId = translate 0 (-50) $ pictures
    [ -- Caja de fondo
      color white       $ rectangleSolid 720 520       -- Borde Blanco
    , color pokemonBlue $ rectangleSolid 700 500       -- Fondo Azul
    , drawPokemonInfo pokemonId                        -- Información del Pokemon
    ]

-- Dibuja la información del Pokemon
drawPokemonInfo :: Int -> Picture
drawPokemonInfo pokemonId = pictures
    [ -- Nombre del Pokemon
      translate (-320) 220 
        $ color pokemonYellow 
        $ scale 0.3 0.3 
        $ text (getPokemonName pokemonId)
    
    , -- Número del Pokemon
      translate (-320) 180 
        $ color white 
        $ scale 0.2 0.2 
        $ text ("#" ++ formatNumber pokemonId)
    
    , -- Stats placeholder (aquí irán las estadísticas)
      translate (-320) 120 
        $ color white 
        $ scale 0.15 0.15 
        $ text "HP:     100"
    
    , translate (-320) 90 
        $ color white 
        $ scale 0.15 0.15 
        $ text "Attack:  80"
    
    , translate (-320) 60 
        $ color white 
        $ scale 0.15 0.15 
        $ text "Defense: 75"
    
    , translate (-320) 30 
        $ color white 
        $ scale 0.15 0.15 
        $ text "Speed:   90"
    
    , -- Tipo del Pokemon
      translate (-320) (-30) 
        $ color pokemonYellow 
        $ scale 0.15 0.15 
        $ text "Type: Normal"
    
    , -- Descripción placeholder
      translate (-320) (-90) 
        $ color white 
        $ scale 0.12 0.12 
        $ text "A powerful Pokemon that can"
    
    , translate (-320) (-110) 
        $ color white 
        $ scale 0.12 0.12 
        $ text "be used in battles."
    ]

-- Obtiene el nombre del Pokemon por su ID
getPokemonName :: Int -> String
getPokemonName pokemonId = case pokemonId of
    1  -> "BULBASAUR"
    2  -> "IVYSAUR"
    3  -> "VENUSAUR"
    4  -> "CHARMANDER"
    5  -> "CHARMELEON"
    6  -> "CHARIZARD"
    7  -> "SQUIRTLE"
    8  -> "WARTORTLE"
    9  -> "BLASTOISE"
    10 -> "CATERPIE"
    11 -> "METAPOD"
    12 -> "BUTTERFREE"
    13 -> "WEEDLE"
    14 -> "KAKUNA"
    15 -> "BEEDRILL"
    16 -> "PIDGEY"
    17 -> "PIDGEOTTO"
    18 -> "PIDGEOT"
    19 -> "RATTATA"
    20 -> "RATICATE"
    _  -> "UNKNOWN"

-- Formatea el número con ceros (ej: 1 -> "001")
formatNumber :: Int -> String
formatNumber n
    | n < 10    = "00" ++ show n
    | n < 100   = "0"  ++ show n
    | otherwise = show n

-- Instrucciones en la parte inferior
drawInstructions :: Picture
drawInstructions = translate 0 (-320) 
    $ color white 
    $ scale 0.15 0.15 
    $ text "BACKSPACE: Return to Pokedex"
