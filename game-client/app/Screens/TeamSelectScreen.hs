module Screens.TeamSelectScreen (drawTeamSelectScreen) where

import qualified Data.Map as Map
import Engine.Common (drawLogo, drawTextWithShadow, pokemonBlue, pokemonYellow)
import Game.Pokemon (Pokemon (..), allPokemon)
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

-- | Dibuja la pantalla de selección de equipo
drawTeamSelectScreen :: Picture -> Picture -> Int -> [Int] -> Map.Map Int Picture -> Picture
drawTeamSelectScreen menuBgImage logoImage selectedId team spriteMap =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      -- 1. CAJA DE SELECCIÓN (Izquierda)
      -- Mantenemos la altura (-50) igual que Pokedex, pero la movemos a la izquierda
      translate (-320) (-50) $ drawSelectionBox selectedId team spriteMap,
      -- 2. CAJA DE EQUIPO (Derecha)
      -- Una nueva caja para mostrar el equipo actual
      translate 320 (-50) $ drawTeamGrid team spriteMap,
      -- 3. INSTRUCCIONES (Footer consistente)
      drawInstructions
    ]

-- ============================================
-- CAJA DE SELECCIÓN (IZQUIERDA)
-- ============================================
-- Esta es la caja tipo Pokedex, pero más angosta para que entre el equipo al lado
drawSelectionBox :: Int -> [Int] -> Map.Map Int Picture -> Picture
drawSelectionBox selectedId team spriteMap =
  pictures
    [ -- Caja Blanca y Azul (Más angosta: 500px en vez de 720px)
      color white $ rectangleSolid 520 440,
      color pokemonBlue $ rectangleSolid 500 420,
      -- Separador vertical
      color white $ translate 0 0 $ rectangleSolid 2 400,
      -- COLUMNA IZQUIERDA: Lista Scrollable
      translate (-120) 0 $ drawScrollingList selectedId team,
      -- COLUMNA DERECHA: Sprite del Pokemon seleccionado
      translate 120 0 $ drawPokemonDisplay selectedId spriteMap
    ]

-- ============================================
-- GRID DE EQUIPO (DERECHA)
-- ============================================
drawTeamGrid :: [Int] -> Map.Map Int Picture -> Picture
drawTeamGrid team spriteMap =
  pictures
    [ -- Caja de fondo para el equipo
      color white $ rectangleSolid 520 440,
      color pokemonBlue $ rectangleSolid 500 420,
      -- Título "YOUR TEAM"
      translate 0 170 $ drawTextWithShadow ("TEAM (" ++ show (length team) ++ "/6)") 0.2 0 white,
      -- Los 6 Slots en Grid (2 columnas x 3 filas)
      drawSlots team spriteMap
    ]

drawSlots :: [Int] -> Map.Map Int Picture -> Picture
drawSlots team spriteMap =
  pictures
    [ -- Fila 1 (Arriba) - Subimos de 80 a 100
      drawTeamSlot spriteMap team 0 (-110) 100,
      drawTeamSlot spriteMap team 1 (110) 100,
      -- Fila 2 (Medio) - Subimos de -40 a -20
      drawTeamSlot spriteMap team 2 (-110) (-20),
      drawTeamSlot spriteMap team 3 (110) (-20),
      drawTeamSlot spriteMap team 4 (-110) (-140),
      drawTeamSlot spriteMap team 5 (110) (-140)
    ]

-- Dibuja un slot individual en una posición específica (X, Y)
drawTeamSlot :: Map.Map Int Picture -> [Int] -> Int -> Float -> Float -> Picture
drawTeamSlot spriteMap team index xPos yPos =
  translate xPos yPos $
    pictures
      [ -- Borde y Fondo del Slot
        color white $ rectangleSolid 180 100,
        color (makeColorI 0 30 80 255) $ rectangleSolid 170 90, -- Un azul más oscuro para los slots

        -- Contenido
        if index < length team
          then drawSlotContent spriteMap (team !! index)
          else drawEmptySlot index
      ]

drawSlotContent :: Map.Map Int Picture -> Int -> Picture
drawSlotContent spriteMap pokemonId =
  case Map.lookup pokemonId spriteMap of
    Just pic -> scale 1.5 1.5 pic -- Pokemon grande
    Nothing -> color red $ circleSolid 20

drawEmptySlot :: Int -> Picture
drawEmptySlot index =
  color (makeColorI 255 255 255 50) $ -- Número gris transparente
    scale 0.3 0.3 $
      translate (-45) (-45) $
        text (show (index + 1))

-- ============================================
-- COMPONENTES REUTILIZADOS (MODIFICADOS LEVEMENTE)
-- ============================================

drawScrollingList :: Int -> [Int] -> Picture
drawScrollingList selectedId team = pictures (zipWith drawEntry visiblePokemon [0 ..])
  where
    maxItems = 10
    selIndex = max 0 (selectedId - 1)
    scrollOffset = max 0 (selIndex - (maxItems `div` 2))
    visiblePokemon = take maxItems $ drop scrollOffset allPokemon

    drawEntry :: Pokemon -> Int -> Picture
    drawEntry p offset =
      let isSelected = pId p == selectedId
          isInTeam = pId p `elem` team
          yPos = 160 - (fromIntegral offset * 35)

          -- Colores
          nameColor
            | isInTeam = makeColorI 100 255 100 255
            | isSelected = white
            | otherwise = makeColorI 180 180 180 255

          -- Texto comprimido porque la caja es más angosta
          txtNum = translate (-110) yPos $ scale 0.12 0.12 $ color pokemonYellow $ text ("#" ++ formatNumber (pId p))
          txtName = translate (-60) yPos $ scale 0.12 0.12 $ color nameColor $ text (take 10 (pName p)) -- Recortamos nombres largos
          cursor =
            if isSelected
              then translate (-125) (yPos + 2) $ color pokemonYellow $ polygon [(0, 0), (0, 10), (8, 5)]
              else blank
       in pictures [cursor, txtNum, txtName]

drawPokemonDisplay :: Int -> Map.Map Int Picture -> Picture
drawPokemonDisplay selectedId spriteMap =
  case Map.lookup selectedId spriteMap of
    Nothing -> scale 0.2 0.2 $ color white $ text "?"
    Just pic -> scale 2.5 2.5 pic

drawInstructions :: Picture
drawInstructions =
  pictures
    [ translate 0 (-320) $
        color (makeColorI 0 0 0 200) $
          rectangleSolid 1280 50,
      drawTextWithShadow
        "ENTER: Add / Continue | BACKSPACE: Remove / Back | R: Random Team"
        0.15
        (-327)
        white
    ]

formatNumber :: Int -> String
formatNumber n
  | n < 10 = "00" ++ show n
  | n < 100 = "0" ++ show n
  | otherwise = show n