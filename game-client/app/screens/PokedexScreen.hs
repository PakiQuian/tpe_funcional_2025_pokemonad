module Screens.PokedexScreen (drawPokedexScreen) where

import Graphics.Gloss
import Engine.Common (pokemonBlue, pokemonYellow, drawLogo, drawCenteredText)
import Game.Pokemon (allPokemon, Pokemon(..))

-- Dibuja la pantalla de Pokedex
-- RECIBE: Fondo -> Logo -> Seleccion -> (NUEVO) SpriteActual -> Resultado
drawPokedexScreen :: Picture -> Picture -> Int -> Maybe Picture -> Picture
drawPokedexScreen menuBgImage logoImage selectedId currentSprite = pictures 
    [ menuBgImage                      
    , drawLogo logoImage               
    , drawPokedexBox selectedId currentSprite
    , drawInstructions                 
    ]

-- Contenedor principal
drawPokedexBox :: Int -> Maybe Picture -> Picture
drawPokedexBox selectedId maybeSprite = translate 0 (-50) $ pictures
    [ -- Caja Blanca Grande
      color white       $ rectangleSolid 720 440
    , color pokemonBlue $ rectangleSolid 700 420
    
    -- SEPARADOR VERTICAL (Linea blanca en el medio)
    , color white $ translate 0 0 $ rectangleSolid 2 400

    -- COLUMNA IZQUIERDA: Lista Scrollable
    , translate (-170) 0 $ drawScrollingList selectedId
    
    -- COLUMNA DERECHA: Sprite del Pokemon
    , translate 170 0 $ drawPokemonDisplay maybeSprite
    ]

-- Dibuja la lista con lógica de "Ventana Deslizante" (Scroll)
drawScrollingList :: Int -> Picture
drawScrollingList selectedId = pictures (zipWith drawEntry visiblePokemon [0..])
  where
    -- CONFIGURACION DEL SCROLL
    maxItems = 10 -- Cuantos pokemons se ven al mismo tiempo
    
    -- El índice real en la lista (restando 1 porque pId empieza en 1)
    selIndex = max 0 (selectedId - 1)
    
    -- Calculamos donde empieza la ventana para que el seleccionado quede al medio
    -- Si selecciono el 5, start=0. Si selecciono el 15, start=10.
    scrollOffset = max 0 (selIndex - (maxItems `div` 2))
    
    -- Recortamos la lista completa 'allPokemon'
    visiblePokemon = take maxItems $ drop scrollOffset allPokemon

    -- DIBUJAR UN RENGLÓN
    drawEntry :: Pokemon -> Int -> Picture
    drawEntry p offset = 
        let 
            -- Es este el seleccionado?
            isSelected = pId p == selectedId
            
            -- Posición Y relativa (va bajando)
            yPos = 160 - (fromIntegral offset * 35)
            
            -- Colores
            nameColor = if isSelected then white else makeColorI 180 180 180 255
            numColor  = pokemonYellow
            
            -- Texto "#001"
            txtNum = translate (-140) yPos 
                   $ scale 0.12 0.12 
                   $ color numColor 
                   $ text ("#" ++ formatNumber (pId p))
            
            -- Texto "BULBASAUR"
            txtName = translate (-90) yPos 
                    $ scale 0.12 0.12 
                    $ color nameColor 
                    $ text (pName p)
            
            -- Triangulito ►
            cursor = if isSelected 
                     then translate (-155) (yPos + 5) 
                          $ color pokemonYellow 
                          $ polygon [(0,0), (0, 10), (8, 5)]
                     else blank
        in
            pictures [cursor, txtNum, txtName]

-- Dibuja la imagen a la derecha
drawPokemonDisplay :: Maybe Picture -> Picture
drawPokemonDisplay Nothing = 
    -- Si no hay imagen, un signo de pregunta
    pictures 
        [ color (makeColorI 0 0 0 100) $ circleSolid 70
        , color white $ scale 0.2 0.2 $ translate (-20) (-20) $ text "?"
        ]
drawPokemonDisplay (Just pic) = 
    -- Escalamos la imagen porque los sprites son chiquitos (64x64)
    scale 3.5 3.5 pic

-- Helpers
formatNumber :: Int -> String
formatNumber n
    | n < 10    = "00" ++ show n
    | n < 100   = "0"  ++ show n
    | otherwise = show n

drawInstructions :: Picture
drawInstructions =
    drawCenteredText 
        "UP/DOWN: Navigate   |   ENTER: Detail   |   BACKSPACE: Menu" 
        0.15    
        (-320)  
        white