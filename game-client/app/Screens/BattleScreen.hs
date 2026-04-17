module Screens.BattleScreen (drawBattleScreen) where

import Data.Char (toUpper)
import qualified Data.Map as Map
import Engine.Common (drawTextWithShadow, pokemonBlue, pokemonYellow)
import Engine.GameState (BattleMenuType (..))
import Game.Battle (BattlePhase (..), BattlePokemon (..), BattleState (..), bpHp, bpMaxHp, bpOriginal)
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
drawBattleScreen :: [Picture] -> Int -> Maybe BattleState -> Map.Map Int Picture -> Map.Map Int Picture -> Int -> BattleMenuType -> Int -> Int -> Picture
drawBattleScreen backgrounds bgIndex maybeState pokemonFrontSprites pokemonBackSprites menuIndex menuType moveIndex benchIndex =
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
              drawBattleLogWindow (battleLog state),
              drawBattleMenu (phase state) (playerActive state) (playerBench state) menuIndex menuType moveIndex benchIndex
            ]

drawBattleLogWindow :: [String] -> Picture
drawBattleLogWindow logs =
  let boxW = 560
      boxH = 220
      maxLines = 8
      shownLogs = map (take 62) (takeLast maxLines logs)
      lineStep = 23
      title = translate (-255) 85 $ scale 0.14 0.14 $ color white $ text "BATTLE LOG"
      linePictures =
        zipWith
          ( \idx ln ->
              translate (-255) (58 - fromIntegral idx * lineStep) $
                scale 0.12 0.12 $
                  color white $
                    text ln
          )
          [0 :: Int ..]
          shownLogs
   in translate (-330) 220 $
        pictures
          [ color (makeColorI 0 0 0 190) $ rectangleSolid boxW boxH,
            color white $ rectangleWire boxW boxH,
            title,
            pictures linePictures
          ]

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - min n (length xs)) xs

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
        translate (-220) 80 $ drawHUD bp
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
        translate (-220) 80 $ drawHUD bp
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

drawBattleMenu :: BattlePhase -> BattlePokemon -> [BattlePokemon] -> Int -> BattleMenuType -> Int -> Int -> Picture
drawBattleMenu battlePhase activePokemon bench menuIdx menuType moveIdx benchIdx =
  translate 0 (-280) $
    if battlePhase == WaitingForForcedPlayerSwitch
      then case menuType of
        SwitchConfirmMenu -> drawSwitchConfirmMenu bench benchIdx moveIdx
        _ -> drawForcedPokemonMenu bench benchIdx
      else case menuType of
        MainBattleMenu -> drawMainMenu activePokemon menuIdx
        FightMenu -> drawFightMenu activePokemon moveIdx
        QuitConfirmMenu -> drawQuitMenu moveIdx
        PokemonMenu -> drawPokemonMenu bench benchIdx
        SwitchConfirmMenu -> drawSwitchConfirmMenu bench benchIdx moveIdx

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
-- 4. MENÚ DE EQUIPO (POKEMON MENU)
-- ===============================================================
drawPokemonMenu :: [BattlePokemon] -> Int -> Picture
drawPokemonMenu bench benchIdx =
  pictures
    [ -- Caja Principal
      color (makeColorI 0 0 0 220) $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      -- Título
      translate (-580) 20 $ scale 0.2 0.2 $ color white $ text "Choose a POKEMON.",
      -- Lista de Pokémon en la banca
      translate (-200) 40 $ pictures $ zipWith (drawBenchSlot benchIdx) [0 ..] bench
    ]

drawForcedPokemonMenu :: [BattlePokemon] -> Int -> Picture
drawForcedPokemonMenu bench benchIdx =
  pictures
    [ color (makeColorI 0 0 0 220) $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      translate (-580) 35 $ scale 0.2 0.2 $ color white $ text "Your Pokemon fainted!",
      translate (-580) 10 $ scale 0.2 0.2 $ color white $ text "Choose a replacement.",
      translate (-200) 40 $ pictures $ zipWith (drawBenchSlot benchIdx) [0 ..] bench
    ]

drawBenchSlot :: Int -> Int -> BattlePokemon -> Picture
drawBenchSlot selectedIndex index bp =
  let isSwitchable = bpHp bp > 0
      isSelected = index == selectedIndex && isSwitchable
      txtColor
        | not isSwitchable = makeColorI 220 80 80 255
        | isSelected = white
        | otherwise = makeColorI 180 180 180 255

      -- Formato: "Pikachu   HP: 35/35"
      nameStr = pName (bpOriginal bp)
      hpStr =
        if isSwitchable
          then "HP: " ++ show (bpHp bp) ++ "/" ++ show (bpMaxHp bp)
          else "HP: 0/" ++ show (bpMaxHp bp) ++ " (FAINTED)"

      -- Posicionamiento en cuadrícula (2 columnas)
      xPos = if even index then 0 else 400
      yPos
        | index < 2 = 0
        | index < 4 = -40
        | otherwise = -80

      txt = translate xPos yPos $ scale 0.18 0.18 $ color txtColor $ text (nameStr ++ "   " ++ hpStr)
      cursor =
        if isSelected
          then translate (xPos - 25) (yPos + 5) $ color pokemonYellow $ polygon [(0, 0), (0, 15), (12, 7.5)]
          else blank
   in pictures [cursor, txt]

-- ===============================================================
-- 5. CONFIRMACIÓN DE CAMBIO (SWITCH)
-- ===============================================================
drawSwitchConfirmMenu :: [BattlePokemon] -> Int -> Int -> Picture
drawSwitchConfirmMenu bench benchIdx moveIdx =
  if benchIdx >= 0 && benchIdx < length bench
    then
      let targetPokemon = bench !! benchIdx
       in pictures
            [ color (makeColorI 0 0 0 220) $ rectangleSolid 1280 160,
              color white $ rectangleWire 1270 150,
              -- Pregunta
              translate (-580) 20 $
                scale 0.25 0.25 $
                  color white $
                    text ("Switch to " ++ pName (bpOriginal targetPokemon) ++ "?"),
              -- Opciones YES / NO (Reutilizamos moveIdx para elegir YES=0, NO=1)
              translate 350 0 $
                pictures
                  [ drawMenuOption 0 "SWITCH" (-120) 0 moveIdx,
                    drawMenuOption 1 "CANCEL" 100 0 moveIdx
                  ]
            ]
    else
      pictures
        [ color (makeColorI 0 0 0 220) $ rectangleSolid 1280 160,
          color white $ rectangleWire 1270 150,
          translate (-580) 20 $
            scale 0.25 0.25 $
              color white $
                text "Invalid Pokemon selection.",
          translate 350 0 $
            pictures
              [ drawMenuOption 0 "SWITCH" (-120) 0 moveIdx,
                drawMenuOption 1 "CANCEL" 100 0 moveIdx
              ]
        ]

-- ===============================================================
-- MENÚ DE CONFIRMACIÓN (QUIT)
-- ===============================================================
drawQuitMenu :: Int -> Picture
drawQuitMenu moveIdx =
  pictures
    [ -- Fondo
      color (makeColorI 0 0 0 220) $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      translate (-580) 20 $
        scale 0.25 0.25 $
          color white $
            text "Are you sure you want to quit?",
      translate 350 0 $
        pictures
          [ drawMenuOption 0 "YES" (-100) 0 moveIdx,
            drawMenuOption 1 "NO" 100 0 moveIdx
          ]
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