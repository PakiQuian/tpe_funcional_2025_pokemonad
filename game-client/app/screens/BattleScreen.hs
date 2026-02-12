module Screens.BattleScreen (drawBattleScreen) where

import qualified Data.Map as Map
import Engine.Common (drawTextWithShadow, pokemonBlue, pokemonYellow)
import Game.Battle (BattlePokemon (..), BattleState (..), bpHp, bpMaxHp, bpOriginal)
import Game.Pokemon (Pokemon (..))
import Graphics.Gloss
  ( Picture,
    black,
    blank,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    rectangleWire,
    scale,
    text,
    translate,
    white,
  )

-- Dibuja la pantalla de batalla
drawBattleScreen :: [Picture] -> Int -> Maybe BattleState -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawBattleScreen backgrounds bgIndex maybeState pokemonFrontSprites pokemonBackSprites =
  let bg =
        if null backgrounds
          then blank
          else backgrounds !! (bgIndex `mod` length backgrounds)
   in case maybeState of
        Nothing -> pictures [bg, drawTextWithShadow "PREPARING BATTLE..." 0.2 0 white]
        Just state ->
          pictures
            [ bg,
              drawEnemyUnit (enemyActive state) pokemonFrontSprites,
              drawPlayerUnit (playerActive state) pokemonBackSprites,
              drawBattleMenu
            ]

-- ===============================================================
-- DIBUJO DE UNIDADES
-- ===============================================================

drawEnemyUnit :: BattlePokemon -> Map.Map Int Picture -> Picture
drawEnemyUnit bp spriteMap =
  translate 360 190 $
    pictures
      [ case Map.lookup (pId (bpOriginal bp)) spriteMap of
          Just pic -> scale 2.5 2.5 pic
          Nothing -> scale 0.2 0.2 $ text "?",
        translate (-300) 100 $ drawHUD bp False
      ]

drawPlayerUnit :: BattlePokemon -> Map.Map Int Picture -> Picture
drawPlayerUnit bp spriteMap =
  translate (-210) (-105) $
    pictures
      [ -- Sprite del Jugador (Back Sprite)
        case Map.lookup (pId (bpOriginal bp)) spriteMap of
          Just pic -> scale 4.5 4.5 pic
          Nothing -> scale 0.2 0.2 $ text "?",
        -- HUD Jugador - Un poco a la derecha
        translate 300 50 $ drawHUD bp True
      ]

-- ===============================================================
-- HUD (Head Up Display - Barras de Vida)
-- ===============================================================

drawHUD :: BattlePokemon -> Bool -> Picture
drawHUD bp isPlayer =
  pictures
    [ -- Caja Blanca semitransparente
      color (makeColorI 255 255 255 200) $ rectangleSolid 260 80,
      color black $ rectangleWire 260 80,
      -- Nombre
      translate (-120) 15 $ scale 0.15 0.15 $ color black $ text (pName (bpOriginal bp)),
      -- Barra de Vida (Fondo Gris)
      translate (-80) (-15) $ color (makeColorI 100 100 100 255) $ rectangleSolid 200 10,
      -- Barra de Vida (Actual - Verde/Amarillo/Rojo)
      let pct = fromIntegral (bpHp bp) / fromIntegral (bpMaxHp bp)
          barColor
            | pct > 0.5 = makeColorI 50 200 50 255 -- Verde
            | pct > 0.2 = makeColorI 200 200 50 255 -- Amarillo
            | otherwise = makeColorI 200 50 50 255 -- Rojo
          width = 200 * pct
       in translate (-80 - (200 - width) / 2) (-15) $ color barColor $ rectangleSolid width 10,
      -- Texto HP (Solo para el jugador)
      if isPlayer
        then translate 40 (-15) $ scale 0.12 0.12 $ color black $ text (show (bpHp bp) ++ "/" ++ show (bpMaxHp bp))
        else blank
    ]

-- ===============================================================
-- MENÚ DE BATALLA (Abajo)
-- ===============================================================

drawBattleMenu :: Picture
drawBattleMenu =
  translate 0 (-280) $
    pictures
      [ color (makeColorI 0 0 0 220) $ rectangleSolid 1280 160,
        color white $ rectangleWire 1270 150,
        translate (-500) 20 $ scale 0.2 0.2 $ color white $ text "What will POKEMON do?"
        -- Aquí agregaremos botones después: FIGHT, BAG, POKEMON, RUN
      ]
