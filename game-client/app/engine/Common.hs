module Engine.Common 
    ( pokemonBlue
    , pokemonYellow
    , drawLogo
    , drawCenteredText
    , loadPngSafe
    ) where

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Data.Maybe (fromMaybe)

-- COLORES PERSONALIZADOS
pokemonBlue :: Color
pokemonBlue = makeColorI 0 50 120 255 

pokemonYellow :: Color
pokemonYellow = makeColorI 255 200 0 255

-- Dibuja el logo en la parte superior
drawLogo :: Picture -> Picture
drawLogo logo = translate 0 250 $ scale 0.8 0.8 logo

-- FUNCIÓN GENÉRICA PARA TEXTO CENTRADO
-- Recibe: Texto -> Escala -> Posición Y -> Color -> Resultado
drawCenteredText :: String -> Float -> Float -> Color -> Picture
drawCenteredText content scl yPos col =
    let
        -- Estimación del ancho de caracter en la fuente por defecto de Gloss
        -- Ajusta este número (55) si sientes que no centra perfecto con tu resolución
        charWidth = 55 
        
        -- Cálculo matemático del centro
        totalWidth = fromIntegral (length content) * charWidth * scl
        xOffset    = -totalWidth / 2
    in
        translate xOffset yPos 
        $ scale scl scl 
        $ color col 
        $ text content

loadPngSafe :: FilePath -> IO Picture
loadPngSafe path = do
    maybePic <- loadJuicyPNG path
    case maybePic of
        Just pic -> return pic
        Nothing  -> do
            putStrLn $ "WARNING: No se pudo cargar la imagen: " ++ path
            -- Devolvemos un placeholder rojo con texto "ERR"
            return $ pictures 
                [ color red $ rectangleSolid 50 50
                , color white $ scale 0.1 0.1 $ translate (-200) 0 $ text "ERR" 
                ]