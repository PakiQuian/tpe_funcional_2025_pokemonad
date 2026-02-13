module Screens.BattleScreen (drawBattleScreen) where

import Data.Char (toUpper)
import qualified Data.Map as Map
import Engine.Common (drawTextWithShadow, pokemonBlue, pokemonYellow)
import Engine.GameState (BattleMenuType (..))
import Game.Battle (BattlePokemon (..), BattleState (..), bpHp, bpMaxHp, bpOriginal)
import Game.Move (Move (..))
import Game.Pokemon (Pokemon (..))
import Graphics.Gloss
  ( Picture,
    black,
    blank,
    color,
    makeColorI,
    pictures,
    polygon,
    rectangleSolid,
    rectangleWire,
    scale,
    text,
    translate,
    white,
  )

-- Dibuja la pantalla de batalla
drawBattleScreen :: [Picture] -> Int -> Maybe BattleState -> Map.Map Int Picture -> Map.Map Int Picture -> Int -> BattleMenuType -> Int -> Picture
drawBattleScreen backgrounds bgIndex maybeState pokemonFrontSprites pokemonBackSprites menuIndex menuType moveIndex =
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
              drawBattleMenu (playerActive state) menuIndex menuType moveIndex
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
        translate (-230) 80 $ drawHUD bp
      ]

drawPlayerUnit :: BattlePokemon -> Map.Map Int Picture -> Picture
drawPlayerUnit bp spriteMap =
  translate (-210) (-105) $
    pictures
      [ case Map.lookup (pId (bpOriginal bp)) spriteMap of
          Just pic -> scale 4.5 4.5 pic
          Nothing -> scale 0.2 0.2 $ text "?",
        translate (-230) 80 $ drawHUD bp
      ]

-- ===============================================================
-- HUD (Head Up Display - Barras de Vida)
-- ===============================================================

drawHUD :: BattlePokemon -> Picture
drawHUD bp =
  pictures
    [ -- Caja Blanca semitransparente
      color (makeColorI 255 255 255 200) $ rectangleSolid 265 65,
      color black $ rectangleWire 265 65,
      -- Nombre
      translate (-120) 8 $ scale 0.15 0.15 $ color black $ text (pName (bpOriginal bp)),
      -- Barra de Vida (Fondo Gris)
      translate (-35) (-13) $ color (makeColorI 100 100 100 255) $ rectangleSolid 170 10,
      -- Barra de Vida (Actual - Verde/Amarillo/Rojo)
      let pct = fromIntegral (bpHp bp) / fromIntegral (bpMaxHp bp)
          barColor
            | pct > 0.5 = makeColorI 50 200 50 255 -- Verde
            | pct > 0.2 = makeColorI 200 200 50 255 -- Amarillo
            | otherwise = makeColorI 200 50 50 255 -- Rojo
          width = 170 * pct
       in translate (-35 - (170 - width) / 2) (-13) $ color barColor $ rectangleSolid width 10,
      translate 60 (-18) $ scale 0.12 0.12 $ color black $ text (show (bpHp bp) ++ "/" ++ show (bpMaxHp bp))
    ]

-- ===============================================================
-- DIRECTOR DEL MENÚ DE BATALLA
-- ===============================================================

drawBattleMenu :: BattlePokemon -> Int -> BattleMenuType -> Int -> Picture
drawBattleMenu activePokemon menuIdx menuType moveIdx = translate 0 (-280) $
  case menuType of
    MainBattleMenu -> drawMainMenu activePokemon menuIdx
    FightMenu -> drawFightMenu activePokemon moveIdx

-- ===============================================================
-- MENÚ PRINCIPAL (What will PKMN do?)
-- ===============================================================

drawMainMenu :: BattlePokemon -> Int -> Picture
drawMainMenu activePokemon menuIdx =
  pictures
    [ color (makeColorI 0 0 0 220) $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      translate (-580) 20 $
        scale 0.25 0.25 $
          color white $
            text ("What will " ++ pName (bpOriginal activePokemon) ++ " do?"),
      translate 350 0 $ drawOptionsGrid menuIdx
    ]

drawOptionsGrid :: Int -> Picture
drawOptionsGrid menuIndex =
  pictures
    [ drawMenuOption 0 "FIGHT" (-150) 25 menuIndex,
      drawMenuOption 1 "BAG" 80 25 menuIndex,
      drawMenuOption 2 "POKEMON" (-150) (-35) menuIndex,
      drawMenuOption 3 "QUIT" 80 (-35) menuIndex
    ]

-- ===============================================================
-- MENÚ DE ATAQUES (FIGHT)
-- ===============================================================
drawFightMenu :: BattlePokemon -> Int -> Picture
drawFightMenu activePokemon moveIdx =
  let moves = bpMoves activePokemon
      -- Conseguir el movimiento seleccionado para mostrar su PP y Tipo
      selectedMove = if moveIdx < length moves then Just (moves !! moveIdx) else Nothing
   in pictures
        [ -- CAJA IZQUIERDA (Los 4 ataques)
          translate (-200) 0 $ color (makeColorI 0 0 0 220) $ rectangleSolid 880 160,
          translate (-200) 0 $ color white $ rectangleWire 870 150,
          translate (-200) 0 $ drawMovesGrid moves moveIdx,
          -- CAJA DERECHA (PP y Tipo)
          translate 450 0 $ color (makeColorI 0 0 0 220) $ rectangleSolid 380 160,
          translate 450 0 $ color white $ rectangleWire 370 150,
          translate 450 0 $ drawMoveDetails selectedMove
        ]

drawMovesGrid :: [Move] -> Int -> Picture
drawMovesGrid moves selectedIndex =
  pictures
    [ drawMoveOption 0 (safeMoveName moves 0) (-300) 25 selectedIndex,
      drawMoveOption 1 (safeMoveName moves 1) 50 25 selectedIndex,
      drawMoveOption 2 (safeMoveName moves 2) (-300) (-35) selectedIndex,
      drawMoveOption 3 (safeMoveName moves 3) 50 (-35) selectedIndex
    ]
  where
    safeMoveName ms i = if i < length ms then mName (ms !! i) else "-"

drawMoveDetails :: Maybe Move -> Picture
drawMoveDetails Nothing = blank
drawMoveDetails (Just move) =
  pictures
    [ translate (-150) 25 $ scale 0.2 0.2 $ color white $ text "PP",
      translate 10 25 $
        scale 0.2 0.2 $
          color white $
            text (show (mPP move) ++ "/" ++ show (mMaxPP move)),
      translate (-150) (-35) $ scale 0.2 0.2 $ color white $ text "TYPE/",
      translate (-10) (-35) $
        scale 0.2 0.2 $
          color white $
            text (map toUpper (show (mType move)))
    ]

-- ===============================================================
-- Cursor
-- ===============================================================

drawMenuOption :: Int -> String -> Float -> Float -> Int -> Picture
drawMenuOption index label xPos yPos selectedIndex =
  let isSelected = index == selectedIndex
      txtColor = if isSelected then white else makeColorI 180 180 180 255

      txt = translate xPos yPos $ scale 0.2 0.2 $ color txtColor $ text label

      cursor =
        if isSelected
          then translate (xPos - 25) (yPos + 3) $ color pokemonYellow $ polygon [(0, 0), (0, 15), (12, 7.5)]
          else blank
   in pictures [cursor, txt]

drawMoveOption :: Int -> String -> Float -> Float -> Int -> Picture
drawMoveOption index label xPos yPos selectedIndex =
  if label == "-"
    then blank -- No dibujar si no hay ataque
    else drawMenuOption index (map toUpper label) xPos yPos selectedIndex