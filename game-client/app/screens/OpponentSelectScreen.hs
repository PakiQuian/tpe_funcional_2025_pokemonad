module Screens.OpponentSelectScreen (drawOpponentSelectScreen) where

import Graphics.Gloss
import Engine.Common (pokemonBlue, pokemonYellow, drawLogo, drawTextWithShadow, drawCenteredText)
import Game.Trainer (allTrainers, Trainer(..))
import qualified Data.Map as Map

-- | Dibuja la pantalla de selección de oponente
-- Recibe: Fondo -> Logo -> Selección actual (índice en allTrainers) -> Picture
drawOpponentSelectScreen :: Picture -> Picture -> Int -> Picture
drawOpponentSelectScreen menuBgImage logoImage selectedIndex = pictures 
    [ menuBgImage                      
    , drawLogo logoImage               
    , drawOpponentBox selectedIndex
    , drawInstructions                 
    ]

-- ============================================
-- CAJA PRINCIPAL DE SELECCIÓN
-- ============================================

drawOpponentBox :: Int -> Picture
drawOpponentBox selectedIndex = translate 0 (-20) $ pictures
    [ -- Caja Blanca Grande
      color white       $ rectangleSolid 720 500
    , color pokemonBlue $ rectangleSolid 700 480
    
    -- Título
    , translate 0 210 $ drawTextWithShadow "CHOOSE YOUR OPPONENT" 0.18 0 white
    
    -- Lista de entrenadores
    , translate (-250) 0 $ drawTrainerList selectedIndex
    
    -- Preview del entrenador seleccionado
    , translate 150 0 $ drawTrainerPreview (allTrainers !! selectedIndex)
    ]

-- ============================================
-- LISTA DE ENTRENADORES
-- ============================================

drawTrainerList :: Int -> Picture
drawTrainerList selectedIndex = pictures $ zipWith drawTrainerEntry allTrainers [0..]
  where
    drawTrainerEntry :: Trainer -> Int -> Picture
    drawTrainerEntry trainer offset = 
        let 
            isSelected = offset == selectedIndex
            yPos = 120 - (fromIntegral offset * 45)
            
            -- Colores
            nameColor = if isSelected then white else makeColorI 180 180 180 255
            
            -- Nombre del entrenador
            txtName = translate 20 yPos 
                    $ scale 0.15 0.15 
                    $ color nameColor 
                    $ text (tName trainer)
            
            -- Cursor
            cursor = if isSelected 
                     then translate 0 (yPos + 2) 
                          $ color pokemonYellow 
                          $ polygon [(0,0), (0, 12), (10, 6)]
                     else blank
            
            -- Dificultad (estrellas)
            stars = translate 20 (yPos - 15)
                  $ scale 0.08 0.08
                  $ color pokemonYellow
                  $ text (difficultyStars (tDifficulty trainer))
        in
            pictures [cursor, txtName, stars]

-- | Convierte el factor de dificultad en estrellas
difficultyStars :: Float -> String
difficultyStars diff
    | diff >= 1.5 = "★★★★★"
    | diff >= 1.3 = "★★★★"
    | diff >= 1.1 = "★★★"
    | diff >= 0.9 = "★★"
    | otherwise   = "★"

-- ============================================
-- PREVIEW DEL ENTRENADOR
-- ============================================

drawTrainerPreview :: Trainer -> Picture
drawTrainerPreview trainer = pictures
    [ -- Marco
      color white $ rectangleSolid 280 400
    , color (makeColorI 30 30 60 255) $ rectangleSolid 270 390
    
    -- Sprite del entrenador (placeholder por ahora)
    , translate 0 100 $ drawTrainerSprite (tSprite trainer)
    
    -- Nombre
    , translate 0 (-50) $ drawCenteredText (tName trainer) 0.12 0 white
    
    -- Equipo
    , translate 0 (-100) $ drawTeamPreview (tTeamIds trainer)
    ]

-- | Dibuja el sprite del entrenador (placeholder)
drawTrainerSprite :: String -> Picture
drawTrainerSprite _spritePath = 
    -- Por ahora un círculo placeholder
    color pokemonYellow $ circleSolid 50

-- | Dibuja una preview del equipo (mini pokébolas)
drawTeamPreview :: [Int] -> Picture
drawTeamPreview teamIds = pictures
    [ drawCenteredText "TEAM:" 0.10 30 (makeColorI 200 200 200 255)
    , pictures $ zipWith drawMiniPokeball [0..5] [-100, -60, -20, 20, 60, 100]
    ]
  where
    -- Dibuja una mini pokébola
    drawMiniPokeball :: Int -> Float -> Picture
    drawMiniPokeball index xPos = translate xPos 0 $ 
        if index < length teamIds
        then pictures
            [ color white $ circleSolid 12
            , color red $ circleSolid 10
            , color white $ rectangleSolid 25 2
            , color white $ circleSolid 3
            ]
        else color (makeColorI 80 80 80 255) $ circleSolid 10

-- ============================================
-- INSTRUCCIONES
-- ============================================

drawInstructions :: Picture
drawInstructions = translate 0 (-330) $ pictures
    [ drawTextWithShadow "ENTER: Start Battle | BACKSPACE: Back to Team Selection" 0.08 0 white
    ]
