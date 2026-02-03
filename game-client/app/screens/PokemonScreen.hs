module Screens.PokemonScreen (drawPokemonScreen) where

import Graphics.Gloss
import Engine.Common (pokemonBlue, pokemonYellow, drawLogo, drawTextWithShadow)
import Game.Pokemon (Pokemon(..), Stats(..), PokemonType(..), getPokemonById)
import Data.List (intercalate)

-- Dibuja la pantalla de detalle
drawPokemonScreen :: Picture -> Picture -> Int -> Maybe Picture -> Picture
drawPokemonScreen menuBgImage logoImage pokemonId maybeSprite = pictures 
    [ menuBgImage                           
    , drawLogo logoImage                    
    , drawDetailBox pokemonId maybeSprite   
    , drawInstructions                      
    ]

-- Contenedor principal
drawDetailBox :: Int -> Maybe Picture -> Picture
drawDetailBox pokemonId maybeSprite = translate 0 (-50) $ pictures
    [ -- CAJA BASE
      color white       $ rectangleSolid 720 440
    , color pokemonBlue $ rectangleSolid 700 420
    
    -- SEPARADOR VERTICAL
    , color white $ translate 0 0 $ rectangleSolid 2 400

    -- COLUMNA IZQUIERDA: Información
    , translate (-170) 0 $ drawPokemonInfo pokemonId
    
    -- COLUMNA DERECHA: Imagen
    , translate 170 0 $ drawPokemonDisplay maybeSprite
    ]

-- Información del Pokemon
drawPokemonInfo :: Int -> Picture
drawPokemonInfo pId = 
    case getPokemonById pId of
        Nothing -> 
            scale 0.2 0.2 $ color white $ text "Pokemon Not Found"
            
        Just p -> pictures
            [ -- 1. HEADER
              translate (-160) 150 $ scale 0.25 0.25 $ color pokemonYellow $ text (pName p)
            , translate (80) 150   $ scale 0.25 0.25 $ color white       $ text ("#" ++ formatNumber pId)
            
            -- 2. TIPOS
            , translate (-160) 110 $ scale 0.15 0.15 $ color white       $ text "TYPE:"
            , translate (-90)  110 $ scale 0.15 0.15 $ color pokemonYellow $ text (showTypes (pType p))

            -- 3. DESCRIPCION
            , translate (-160) 60  $ scale 0.12 0.12 $ color white $ text (take 35 (pDescription p))
            , translate (-160) 40  $ scale 0.12 0.12 $ color white $ text (drop 35 (pDescription p))

            -- 4. STATS (Tabla mejorada)
            , drawStatsTable (pStats p)
            ]

-- Dibuja la grilla de 6 estadísticas
drawStatsTable :: Stats -> Picture
drawStatsTable s = translate (-160) (-20) $ pictures
    [ -- Columna Izquierda
      drawStatRow 0  "HP"      (hp s)
    , drawStatRow 1  "Attack"  (attack s)
    , drawStatRow 2  "Defense" (defense s)
    
    -- Columna Derecha (Movemos 160px a la derecha)
    , translate 160 0 $ pictures 
        [ drawStatRow 0 "Sp. Atk" (specialAttack s)
        , drawStatRow 1 "Sp. Def" (specialDefense s)
        , drawStatRow 2 "Speed"   (speed s)
        ]
    ]

-- Dibuja una fila con BARRA ALINEADA y COLOREADA
drawStatRow :: Int -> String -> Int -> Picture
drawStatRow row label val = 
    let 
        yPos = fromIntegral (row * (-45))
        
        -- ESCALADO (DENSIDAD)
        -- Multiplicamos por 0.6 para que un stat de 250 ocupe 150px
        barWidth = fromIntegral val * 0.6 
        
        -- El ancho de la barra de fondo vuelve a ser estético (150px)
        maxBarWidth = 150                 
        
        -- COLORES (Nuevos Rangos: Rojo -> Naranja -> Amarillo -> Cyan -> Violeta)
        statColor 
            | val < 50  = makeColorI 255 80 80 255    -- Rojo (Muy Bajo)
            | val < 80  = makeColorI 255 140 0 255    -- Naranja (Bajo)
            | val < 110 = makeColorI 255 200 0 255    -- Amarillo (Promedio)
            | val < 150 = makeColorI 0 255 255 255    -- Cyan (Bueno)
            | otherwise = makeColorI 180 60 255 255   -- Violeta (Epico - Ej: Cloyster Def)

    in pictures 
        [ -- 1. Etiqueta
          translate 0 yPos 
          $ scale 0.12 0.12 
          $ color white 
          $ text label
          
        , -- 2. Valor numérico
          translate 80 yPos 
          $ scale 0.12 0.12 
          $ color statColor 
          $ text (show val)

        , -- 3. BARRAS
          translate 0 (yPos - 15) $ pictures 
            [ 
              -- A. Barra de Fondo (Gris, ancho fijo 150px)
              translate (maxBarWidth / 2) 0 
              $ color (makeColorI 50 50 50 255) 
              $ rectangleSolid maxBarWidth 6
            
            , -- B. Barra de Valor (Escalada y coloreada)
              translate (barWidth / 2) 0 
              $ color statColor 
              $ rectangleSolid barWidth 6
            ]
        ]

-- Dibuja la imagen
drawPokemonDisplay :: Maybe Picture -> Picture
drawPokemonDisplay Nothing = 
    pictures [ color (makeColorI 0 0 0 100) $ circleSolid 70
             , color white $ scale 0.2 0.2 $ translate (-20) (-20) $ text "?" ]
drawPokemonDisplay (Just pic) = scale 3.5 3.5 pic

-- Helpers
formatNumber :: Int -> String
formatNumber n
    | n < 10    = "00" ++ show n
    | n < 100   = "0"  ++ show n
    | otherwise = show n

showTypes :: [PokemonType] -> String
showTypes ts = intercalate " / " (map show ts)

drawInstructions :: Picture
drawInstructions = pictures
    [ translate 0 (-320) 
      $ color (makeColorI 0 0 0 200) 
      $ rectangleSolid 1280 50

    , drawTextWithShadow 
        "BACKSPACE: Return to Pokedex" 
        0.15    
        (-327)
        white
    ]