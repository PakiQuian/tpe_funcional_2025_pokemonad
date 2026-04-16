module Screens.OpponentSelectScreen (drawOpponentSelectScreen) where

import qualified Data.Map as Map
import Engine.Common (drawCenteredText, drawLogo, drawTextWithShadow, pokemonBlue, pokemonYellow)
import Game.Trainer (AIDifficulty (..), Trainer (..), allTrainers)
import Graphics.Gloss
  ( Picture,
    blank,
    circleSolid,
    color,
    makeColorI,
    pictures,
    polygon,
    rectangleSolid,
    red,
    scale,
    text,
    translate,
    white,
  )

-- | Dibuja la pantalla de selección de oponente
drawOpponentSelectScreen :: Picture -> Picture -> Int -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawOpponentSelectScreen menuBgImage logoImage selectedIndex pokemonSpriteMap trainerSpriteMap =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      drawOpponentBox selectedIndex pokemonSpriteMap trainerSpriteMap,
      drawInstructions
    ]

-- ============================================
-- CAJA PRINCIPAL DE SELECCIÓN
-- ============================================

drawOpponentBox :: Int -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawOpponentBox selectedIndex pokemonSpriteMap trainerSpriteMap =
  translate 0 (-50) $
    pictures
      [ -- Caja Blanca Grande
        color white $ rectangleSolid 720 440,
        color pokemonBlue $ rectangleSolid 700 420,
        -- Separador Vertical
        color white $ translate 0 (-20) $ rectangleSolid 2 350,
        -- Título
        translate 0 170 $ drawTextWithShadow "CHOOSE YOUR OPPONENT" 0.18 0 white,
        -- Lista de entrenadores (Izquierda)
        translate (-170) 0 $ drawTrainerList selectedIndex,
        -- Preview del entrenador (Derecha)
        translate 170 0 $ drawTrainerPreview (allTrainers !! selectedIndex) pokemonSpriteMap trainerSpriteMap
      ]

-- ============================================
-- LISTA DE ENTRENADORES (IZQUIERDA)
-- ============================================

drawTrainerList :: Int -> Picture
drawTrainerList selectedIndex = pictures $ zipWith drawTrainerEntry visibleTrainers [0 ..]
  where
    maxItems = 8
    scrollOffset = max 0 (selectedIndex - (maxItems `div` 2))
    visibleTrainers = take maxItems $ drop scrollOffset allTrainers

    drawTrainerEntry :: Trainer -> Int -> Picture
    drawTrainerEntry trainer offset =
      let realIndex = scrollOffset + offset
          isSelected = realIndex == selectedIndex

          yPos = 120 - (fromIntegral offset * 45)
          nameColor = if isSelected then white else makeColorI 180 180 180 255

          -- 1. NOMBRE (Alineado más a la izquierda)
          -- Antes era -90, ahora -140 para pegarlo al borde
          txtName =
            translate (-140) yPos $
              scale 0.15 0.15 $
                color nameColor $
                  text (tName trainer)

          -- 2. CURSOR
          cursor =
            if isSelected
              then
                translate (-160) (yPos + 2) $
                  color pokemonYellow $
                    polygon [(0, 0), (0, 12), (10, 6)]
              else blank

          -- 3. DIFICULTAD (Estrellas)
          stars =
            translate (-140) (yPos - 15) $
              scale 0.12 0.12 $
                color pokemonYellow $
                  text (difficultyStars (tDifficulty trainer))
       in pictures [cursor, txtName, stars]

difficultyStars :: AIDifficulty -> String
difficultyStars DifficultyEasy = "**"
difficultyStars DifficultyMedium = "***"
difficultyStars DifficultyHard = "*****"
difficultyStars DifficultyExtreme = "*******"

-- ============================================
-- PREVIEW DEL ENTRENADOR (DERECHA)
-- ============================================

drawTrainerPreview :: Trainer -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawTrainerPreview trainer pokemonSpriteMap trainerSpriteMap =
  let isRandom = null (tTeamIds trainer)
   in pictures
        [ -- 1. SPRITE DEL ENTRENADOR
          if isRandom
            -- Si es Random: CENTRADO ABSOLUTO en la caja derecha
            then translate 0 0 $ drawTrainerSprite (tId trainer) trainerSpriteMap
            -- Si es Normal: Arriba para dejar lugar al equipo
            else translate 0 70 $ drawTrainerSprite (tId trainer) trainerSpriteMap,
          -- 2. NOMBRE
          if isRandom
            then translate 0 (-100) $ drawCenteredText (tName trainer) 0.18 0 white
            else translate 0 0 $ drawCenteredText (tName trainer) 0.15 0 white,
          -- 3. EQUIPO (Solo si NO es random)
          if isRandom
            then blank
            else translate 0 (-110) $ drawMiniTeamGrid (tTeamIds trainer) pokemonSpriteMap
        ]

-- Busca la imagen del entrenador en el mapa
drawTrainerSprite :: Int -> Map.Map Int Picture -> Picture
drawTrainerSprite trainerId spriteMap =
  case Map.lookup trainerId spriteMap of
    Just pic -> scale 2.5 2.5 pic
    Nothing ->
      pictures
        [ color (makeColorI 255 255 255 50) $ circleSolid 60,
          scale 0.1 0.1 $ translate (-100) 0 $ color white $ text "NO IMG"
        ]

-- ============================================
-- EQUIPO MINI (GRID)
-- ============================================

drawMiniTeamGrid :: [Int] -> Map.Map Int Picture -> Picture
drawMiniTeamGrid teamIds spriteMap =
  pictures
    [ drawCenteredText "TEAM PREVIEW" 0.10 90 pokemonYellow,
      pictures $ zipWith (drawMiniSlot spriteMap) [0 ..] teamIds
    ]

drawMiniSlot :: Map.Map Int Picture -> Int -> Int -> Picture
drawMiniSlot spriteMap index pokemonId =
  let col = index `mod` 3
      row = index `div` 3
      xPos = -70 + (fromIntegral col * 70)
      yPos = 30 - (fromIntegral row * 70)
   in translate xPos yPos $ case Map.lookup pokemonId spriteMap of
        Nothing -> scale 0.1 0.1 $ color red $ text "?"
        Just pic -> scale 0.7 0.7 pic

-- ============================================
-- INSTRUCCIONES
-- ============================================

drawInstructions :: Picture
drawInstructions =
  pictures
    [ translate 0 (-320) $ color (makeColorI 0 0 0 200) $ rectangleSolid 1280 50,
      drawTextWithShadow "ENTER: Start Battle | BACKSPACE: Back" 0.15 (-327) white
    ]
