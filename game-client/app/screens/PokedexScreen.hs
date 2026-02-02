module Screens.PokedexScreen (drawPokedexScreen) where

import Graphics.Gloss
import Engine.Common (pokemonBlue, pokemonYellow, drawLogo, drawCenteredText)

-- Lista de Pokemon (Igual que antes...)
pokemonList :: [String]
pokemonList = 
    [ "BULBASAUR", "IVYSAUR", "VENUSAUR", "CHARMANDER", "CHARMELEON", "CHARIZARD"
    , "SQUIRTLE", "WARTORTLE", "BLASTOISE", "CATERPIE", "METAPOD", "BUTTERFREE"
    , "WEEDLE", "KAKUNA", "BEEDRILL", "PIDGEY", "PIDGEOTTO", "PIDGEOT"
    , "RATTATA", "RATICATE"
    ]

drawPokedexScreen :: Picture -> Picture -> Int -> Picture
drawPokedexScreen menuBgImage logoImage selectedPokemon = pictures 
    [ menuBgImage                      
    , drawLogo logoImage               
    , drawPokedexBox pokemonList selectedPokemon 
    , drawInstructions                 
    ]

-- Contenedor principal del Pokedex (más grande que el del menú)
drawPokedexBox :: [String] -> Int -> Picture
drawPokedexBox pokemons selectedPokemon = translate 0 (-50) $ pictures
    [ -- Caja de fondo del Pokedex (más grande para la lista)
      color white       $ rectangleSolid 720 440
    , color pokemonBlue $ rectangleSolid 700 420
    , drawPokemonList pokemons selectedPokemon
    ]

-- Dibuja la lista de Pokemon en 2 columnas
drawPokemonList :: [String] -> Int -> Picture
drawPokemonList pokemons selectedPokemon = 
    let 
        leftColumn  = take 10 pokemons
        rightColumn = drop 10 pokemons
        
        leftPics  = zipWith (drawPokemonEntry selectedPokemon (-280)) [1..] leftColumn
        rightPics = zipWith (drawPokemonEntry selectedPokemon 50) [11..] rightColumn
    in
        pictures (leftPics ++ rightPics)

-- Dibuja una entrada de Pokemon (Número + Nombre + Cursor)
drawPokemonEntry :: Int -> Float -> Int -> String -> Picture
drawPokemonEntry selectedPokemon xOffset index name =
    let 
        yPos = 160 - fromIntegral ((index - 1) `mod` 10) * 35
        isSelected = selectedPokemon == index
        numColor = pokemonYellow
        nameColor = if isSelected then white else makeColorI 180 180 180 255
        
        -- Número del Pokemon (ej: "#001")
        pokemonNum = translate xOffset yPos 
                   $ scale 0.12 0.12 
                   $ color numColor 
                   $ text ("#" ++ formatNumber index)
        
        -- Nombre del Pokemon
        pokemonName = translate (xOffset + 40) yPos
                    $ scale 0.12 0.12
                    $ color nameColor
                    $ text name
        
        -- Cursor (triángulo ►) cuando está seleccionado
        cursor = if isSelected 
                 then translate (xOffset - 15) (yPos + 5) 
                      $ color pokemonYellow 
                      $ polygon [(0,0), (0, 10), (8, 5)]
                 else blank
    in
        pictures [cursor, pokemonNum, pokemonName]

-- Formatea el número con ceros (ej: 1 -> "001")
formatNumber :: Int -> String
formatNumber n
    | n < 10    = "00" ++ show n
    | n < 100   = "0"  ++ show n
    | otherwise = show n

drawInstructions :: Picture
drawInstructions =
    drawCenteredText 
        "UP/DOWN: Navigate   |   ENTER: Stats   |   BACKSPACE: Menu" 
        0.15    -- Escala
        (-320)  -- Posición Y
        white   -- Color
