module Screens.OpponentSelectScreen (drawOpponentSelectScreen) where

import Graphics.Gloss
import Engine.Common (pokemonBlue, pokemonYellow, drawLogo, drawTextWithShadow, drawCenteredText)
import Game.Trainer (allTrainers, Trainer(..))
import qualified Data.Map as Map

-- | Dibuja la pantalla de selección de oponente
drawOpponentSelectScreen :: Picture -> Picture -> Int -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawOpponentSelectScreen menuBgImage logoImage selectedIndex pokemonSpriteMap trainerSpriteMap = pictures 
    [ menuBgImage                      
    , drawLogo logoImage               
    , drawOpponentBox selectedIndex pokemonSpriteMap trainerSpriteMap
    , drawInstructions                 
    ]

-- ============================================
-- CAJA PRINCIPAL DE SELECCIÓN
-- ============================================

drawOpponentBox :: Int -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawOpponentBox selectedIndex pokemonSpriteMap trainerSpriteMap = translate 0 (-50) $ pictures
    [ -- Caja Blanca Grande (Ahora 440 de alto, igual que las otras)
      color white       $ rectangleSolid 720 440
    , color pokemonBlue $ rectangleSolid 700 420
    
    -- Separador Vertical
    , color white $ translate 0 0 $ rectangleSolid 2 300
    
    -- Título (Lo bajamos un poco para que entre bien en la nueva altura)
    , translate 0 170 $ drawTextWithShadow "CHOOSE YOUR OPPONENT" 0.18 0 white
    
    -- Lista de entrenadores (Izquierda - Alineada igual que Pokedex)
    , translate (-170) 0 $ drawTrainerList selectedIndex
    
    -- Preview del entrenador (Derecha)
    , translate 170 0 $ drawTrainerPreview (allTrainers !! selectedIndex) pokemonSpriteMap trainerSpriteMap
    ]

-- ============================================
-- LISTA DE ENTRENADORES (IZQUIERDA)
-- ============================================

drawTrainerList :: Int -> Picture
drawTrainerList selectedIndex = pictures $ zipWith drawTrainerEntry visibleTrainers [0..]
  where
    -- Lógica de Scroll simple (ventana de 8 entrenadores)
    maxItems = 8
    scrollOffset = max 0 (selectedIndex - (maxItems `div` 2))
    visibleTrainers = take maxItems $ drop scrollOffset allTrainers

    drawTrainerEntry :: Trainer -> Int -> Picture
    drawTrainerEntry trainer offset = 
        let 
            -- Calculamos si es el seleccionado real (sumando el offset)
            realIndex = scrollOffset + offset
            isSelected = realIndex == selectedIndex
            
            yPos = 120 - (fromIntegral offset * 45)
            nameColor = if isSelected then white else makeColorI 180 180 180 255
            
            -- Nombre del entrenador
            txtName = translate (-90) yPos -- Pegado a la izquierda del panel
                    $ scale 0.15 0.15 
                    $ color nameColor 
                    $ text (tName trainer)
            
            -- Cursor
            cursor = if isSelected 
                     then translate (-110) (yPos + 2) $ color pokemonYellow $ polygon [(0,0), (0, 12), (10, 6)]
                     else blank
            
            -- Estrellas (Dificultad)
            stars = translate (-90) (yPos - 15) $ scale 0.08 0.08 $ color pokemonYellow $ text (difficultyStars (tDifficulty trainer))
        in
            pictures [cursor, txtName, stars]

difficultyStars :: Float -> String
difficultyStars diff
    | diff >= 1.5 = "★★★★★"
    | diff >= 1.3 = "★★★★"
    | diff >= 1.1 = "★★★"
    | diff >= 0.9 = "★★"
    | otherwise   = "★"

-- ============================================
-- PREVIEW DEL ENTRENADOR (DERECHA)
-- ============================================

drawTrainerPreview :: Trainer -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawTrainerPreview trainer pokemonSpriteMap trainerSpriteMap = 
    let 
        -- Detectamos si es "Random" o un entrenador especial sin equipo definido
        isRandom = null (tTeamIds trainer)
    in 
        pictures
        [ -- Caja contenedora de la derecha (opcional, ayuda a encuadrar)
          -- color (makeColorI 0 0 0 50) $ rectangleSolid 300 350
        
          -- 1. SPRITE DEL ENTRENADOR
          if isRandom
             then translate 0 0   $ drawTrainerSprite (tId trainer) trainerSpriteMap -- Centrado absoluto
             else translate 0 70  $ drawTrainerSprite (tId trainer) trainerSpriteMap -- Arriba
        
          -- 2. NOMBRE (Solo si no es random, o ajustado)
        , if isRandom
             then translate 0 (-100) $ drawCenteredText (tName trainer) 0.18 0 white -- Texto más grande y centrado abajo
             else translate 0 0      $ drawCenteredText (tName trainer) 0.15 0 white
        
          -- 3. EQUIPO (Solo si NO es random)
        , if isRandom 
             then blank 
             else translate 0 (-110) $ drawMiniTeamGrid (tTeamIds trainer) pokemonSpriteMap
        ]

-- Busca la imagen del entrenador en el mapa
drawTrainerSprite :: Int -> Map.Map Int Picture -> Picture
drawTrainerSprite trainerId spriteMap = 
    case Map.lookup trainerId spriteMap of
        -- Sprites grandes (2.5x)
        Just pic -> scale 2.5 2.5 pic 
        Nothing  -> pictures 
            [ color (makeColorI 255 255 255 50) $ circleSolid 60
            , scale 0.1 0.1 $ translate (-100) 0 $ color white $ text "NO IMG"
            ]

-- ============================================
-- EQUIPO MINI (GRID)
-- ============================================

drawMiniTeamGrid :: [Int] -> Map.Map Int Picture -> Picture
drawMiniTeamGrid teamIds spriteMap = pictures
    [ drawCenteredText "TEAM PREVIEW" 0.10 90 pokemonYellow
    , pictures $ zipWith (drawMiniSlot spriteMap) [0..] teamIds
    ]

drawMiniSlot :: Map.Map Int Picture -> Int -> Int -> Picture
drawMiniSlot spriteMap index pokemonId = 
    let
        col = index `mod` 3
        row = index `div` 3
        
        -- Posiciones compactas
        xPos = -70 + (fromIntegral col * 70) 
        yPos = 30 - (fromIntegral row * 70)
    in
        translate xPos yPos $ case Map.lookup pokemonId spriteMap of
            Nothing  -> scale 0.1 0.1 $ color red $ text "?"
            Just pic -> scale 0.7 0.7 pic 

-- ============================================
-- INSTRUCCIONES
-- ============================================

drawInstructions :: Picture
drawInstructions = pictures
    [ translate 0 (-320) $ color (makeColorI 0 0 0 200) $ rectangleSolid 1280 50
    , drawTextWithShadow "ENTER: Start Battle | BACKSPACE: Back" 0.15 (-327) white
    ]