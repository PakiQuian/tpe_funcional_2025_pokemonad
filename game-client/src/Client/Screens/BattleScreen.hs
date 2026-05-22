module Client.Screens.BattleScreen (drawBattleScreen) where

import Client.Drawing
  ( cursorYellowColor,
    dimTextColor,
    drawTextWithShadow,
    hpBarGreenColor,
    hpBarRedColor,
    hpBarYellowColor,
    hpTrackColor,
    hudBgColor,
    overlayDarkColor,
    overlayMedColor,
  )
import Client.Types (BattleMenuType (..))
import Data.Char (toUpper)
import qualified Data.Map as Map
import Graphics.Gloss
  ( Picture,
    black,
    blank,
    color,
    pictures,
    polygon,
    rectangleSolid,
    rectangleWire,
    scale,
    text,
    translate,
    white,
  )
import Pokemonad.Battle.State (BattlePhase (..), BattlePokemon (..), BattleState (..), Side (..))
import Pokemonad.Core.Move (Move (..))
import Pokemonad.Core.Pokemon (Pokemon (..))
import Pokemonad.Core.Types (HP (..), PokemonId (..))

drawBattleScreen :: [Picture] -> Int -> Maybe BattleState -> Map.Map PokemonId Picture -> Map.Map PokemonId Picture -> Int -> BattleMenuType -> Int -> Int -> Maybe Side -> Float -> Maybe String -> Picture
drawBattleScreen backgrounds bgIndex maybeState pokeFrontSprites pokeBackSprites menuIndex menuType moveIndex benchIndex shakeTarget shakeTimer waitingMsg =
  let bg = if null backgrounds then blank else backgrounds !! (bgIndex `mod` length backgrounds)
      enemyOffset = shakeOffsetFor EnemySide shakeTarget shakeTimer
      playerOffset = shakeOffsetFor PlayerSide shakeTarget shakeTimer
   in case maybeState of
        Nothing -> pictures [bg, drawTextWithShadow "PREPARING BATTLE..." 0.2 0 white]
        Just state ->
          pictures
            [ bg,
              drawEnemyUnit (enemyActive state) pokeFrontSprites enemyOffset,
              drawPlayerUnit (playerActive state) pokeBackSprites playerOffset,
              drawBattleLogWindow (battleLog state),
              case waitingMsg of
                Just msg -> drawWaitingMessage msg
                Nothing -> drawBattleMenu (phase state) (playerActive state) (playerBench state) menuIndex menuType moveIndex benchIndex
            ]

-- | Replaces the action menu while we're waiting on the opponent in MP.
--   The message is centered horizontally using a rough char-width estimate.
drawWaitingMessage :: String -> Picture
drawWaitingMessage msg =
  let charWidthAtScale = 15 -- approx half-width per char at scale 0.3
      xOffset = -fromIntegral (length msg) * charWidthAtScale
   in translate 0 (-280) $
        pictures
          [ color overlayDarkColor $ rectangleSolid 1280 160,
            color white $ rectangleWire 1270 150,
            translate xOffset (-10) $ scale 0.3 0.3 $ color white $ text msg
          ]

-- | Horizontal pixel offset for a sprite when its side is the shake target.
--   The offset oscillates and decays to 0 as the timer reaches 0.
shakeOffsetFor :: Side -> Maybe Side -> Float -> Float
shakeOffsetFor side (Just target) timer
  | side == target && timer > 0 = sin (timer * 60) * 8
shakeOffsetFor _ _ _ = 0

drawBattleLogWindow :: [String] -> Picture
drawBattleLogWindow logs =
  let maxLines = 8
      shownLogs = map (take 62) (takeLast maxLines logs)
      lineStep = 23
      title = translate (-255) 85 $ scale 0.14 0.14 $ color white $ text "BATTLE LOG"
      linePictures =
        zipWith
          (\idx ln -> translate (-255) (58 - fromIntegral idx * lineStep) $ scale 0.12 0.12 $ color white $ text ln)
          [0 :: Int ..]
          shownLogs
   in translate (-330) 220 $
        pictures
          [ color overlayMedColor $ rectangleSolid 560 220,
            color white $ rectangleWire 560 220,
            title,
            pictures linePictures
          ]

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - min n (length xs)) xs

drawEnemyUnit :: BattlePokemon -> Map.Map PokemonId Picture -> Float -> Picture
drawEnemyUnit bp spriteMap shakeOffset =
  translate 360 190 $
    pictures
      [ translate shakeOffset 0 $ case Map.lookup (pokemonId (battlePokemonBase bp)) spriteMap of
          Just pic -> scale 2.5 2.5 pic
          Nothing -> scale 0.2 0.2 $ text "?",
        translate (-220) 80 $ drawHUD bp
      ]

drawPlayerUnit :: BattlePokemon -> Map.Map PokemonId Picture -> Float -> Picture
drawPlayerUnit bp spriteMap shakeOffset =
  translate (-210) (-105) $
    pictures
      [ translate shakeOffset 0 $ case Map.lookup (pokemonId (battlePokemonBase bp)) spriteMap of
          Just pic -> scale 4.5 4.5 pic
          Nothing -> scale 0.2 0.2 $ text "?",
        translate (-220) 80 $ drawHUD bp
      ]

drawHUD :: BattlePokemon -> Picture
drawHUD bp =
  let currentHp = unHP (battlePokemonHp bp)
      maxHp = unHP (battlePokemonMaxHp bp)
      pct = fromIntegral currentHp / fromIntegral (max 1 maxHp) :: Float
      barColor
        | pct > 0.5 = hpBarGreenColor
        | pct > 0.2 = hpBarYellowColor
        | otherwise = hpBarRedColor
      width = 170 * pct
   in pictures
        [ color hudBgColor $ rectangleSolid 265 65,
          color black $ rectangleWire 265 65,
          translate (-120) 8 $ scale 0.15 0.15 $ color black $ text (pokemonName (battlePokemonBase bp)),
          translate (-35) (-13) $ color hpTrackColor $ rectangleSolid 170 10,
          translate (-35 - (170 - width) / 2) (-13) $ color barColor $ rectangleSolid width 10,
          translate 60 (-18) $ scale 0.12 0.12 $ color black $ text (show currentHp ++ "/" ++ show maxHp)
        ]

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

drawMainMenu :: BattlePokemon -> Int -> Picture
drawMainMenu activePokemon menuIdx =
  pictures
    [ color overlayDarkColor $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      translate (-580) 20 $ scale 0.25 0.25 $ color white $ text ("What will " ++ pokemonName (battlePokemonBase activePokemon) ++ " do?"),
      translate 350 0 $ drawOptionsGrid menuIdx
    ]

drawOptionsGrid :: Int -> Picture
drawOptionsGrid menuIndex =
  pictures
    [ drawMenuOption 0 "FIGHT" (-220) 0 menuIndex,
      drawMenuOption 1 "POKEMON" (-50) 0 menuIndex,
      drawMenuOption 2 "QUIT" 180 0 menuIndex
    ]

drawFightMenu :: BattlePokemon -> Int -> Picture
drawFightMenu activePokemon moveIdx =
  let moves = battlePokemonMoves activePokemon
      selectedMove = if moveIdx < length moves then Just (moves !! moveIdx) else Nothing
   in pictures
        [ translate (-200) 0 $ color overlayDarkColor $ rectangleSolid 880 160,
          translate (-200) 0 $ color white $ rectangleWire 870 150,
          translate (-200) 0 $ drawMovesGrid moves moveIdx,
          translate 450 0 $ color overlayDarkColor $ rectangleSolid 380 160,
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
    safeMoveName ms i = if i < length ms then moveName (ms !! i) else "-"

drawMoveDetails :: Maybe Move -> Picture
drawMoveDetails Nothing = blank
drawMoveDetails (Just move) =
  pictures
    [ translate (-150) 25 $ scale 0.2 0.2 $ color white $ text "PP",
      translate 10 25 $ scale 0.2 0.2 $ color white $ text (show (movePP move) ++ "/" ++ show (moveMaxPP move)),
      translate (-150) (-35) $ scale 0.2 0.2 $ color white $ text "TYPE/",
      translate (-10) (-35) $ scale 0.2 0.2 $ color white $ text (map toUpper (show (moveType move)))
    ]

drawPokemonMenu :: [BattlePokemon] -> Int -> Picture
drawPokemonMenu bench benchIdx =
  pictures
    [ color overlayDarkColor $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      translate (-580) 20 $ scale 0.2 0.2 $ color white $ text "Choose a POKEMON.",
      translate (-200) 40 $ pictures $ zipWith (drawBenchSlot benchIdx) [0 ..] bench
    ]

drawForcedPokemonMenu :: [BattlePokemon] -> Int -> Picture
drawForcedPokemonMenu bench benchIdx =
  pictures
    [ color overlayDarkColor $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      translate (-580) 35 $ scale 0.2 0.2 $ color white $ text "Your Pokemon fainted!",
      translate (-580) 10 $ scale 0.2 0.2 $ color white $ text "Choose a replacement.",
      translate (-200) 40 $ pictures $ zipWith (drawBenchSlot benchIdx) [0 ..] bench
    ]

drawBenchSlot :: Int -> Int -> BattlePokemon -> Picture
drawBenchSlot selectedIndex index bp =
  let currentHp = unHP (battlePokemonHp bp)
      maxHp = unHP (battlePokemonMaxHp bp)
      isSwitchable = currentHp > 0
      isSelected = index == selectedIndex && isSwitchable
      txtColor
        | not isSwitchable = hpBarRedColor
        | isSelected = white
        | otherwise = dimTextColor
      nameStr = pokemonName (battlePokemonBase bp)
      hpStr =
        if isSwitchable
          then "HP: " ++ show currentHp ++ "/" ++ show maxHp
          else "HP: 0/" ++ show maxHp ++ " (FAINTED)"
      xPos = if even index then 0 else 400 :: Float
      yPos
        | index < 2 = 0
        | index < 4 = -40
        | otherwise = -80 :: Float
      txt = translate xPos yPos $ scale 0.18 0.18 $ color txtColor $ text (nameStr ++ "   " ++ hpStr)
      cursor = if isSelected then translate (xPos - 25) (yPos + 5) $ color cursorYellowColor $ polygon [(0, 0), (0, 15), (12, 7.5)] else blank
   in pictures [cursor, txt]

drawSwitchConfirmMenu :: [BattlePokemon] -> Int -> Int -> Picture
drawSwitchConfirmMenu bench benchIdx moveIdx =
  let targetName =
        if benchIdx >= 0 && benchIdx < length bench
          then pokemonName (battlePokemonBase (bench !! benchIdx))
          else "???"
   in pictures
        [ color overlayDarkColor $ rectangleSolid 1280 160,
          color white $ rectangleWire 1270 150,
          translate (-580) 20 $ scale 0.25 0.25 $ color white $ text ("Switch to " ++ targetName ++ "?"),
          translate 350 0 $
            pictures
              [ drawMenuOption 0 "SWITCH" (-120) 0 moveIdx,
                drawMenuOption 1 "CANCEL" 100 0 moveIdx
              ]
        ]

drawQuitMenu :: Int -> Picture
drawQuitMenu moveIdx =
  pictures
    [ color overlayDarkColor $ rectangleSolid 1280 160,
      color white $ rectangleWire 1270 150,
      translate (-580) 20 $ scale 0.25 0.25 $ color white $ text "Are you sure you want to quit?",
      translate 350 0 $
        pictures
          [ drawMenuOption 0 "YES" (-100) 0 moveIdx,
            drawMenuOption 1 "NO" 100 0 moveIdx
          ]
    ]

drawMenuOption :: Int -> String -> Float -> Float -> Int -> Picture
drawMenuOption index label xPos yPos selectedIndex =
  let isSelected = index == selectedIndex
      txtColor = if isSelected then white else dimTextColor
      txt = translate xPos yPos $ scale 0.2 0.2 $ color txtColor $ text label
      cursor = if isSelected then translate (xPos - 25) (yPos + 3) $ color cursorYellowColor $ polygon [(0, 0), (0, 15), (12, 7.5)] else blank
   in pictures [cursor, txt]

drawMoveOption :: Int -> String -> Float -> Float -> Int -> Picture
drawMoveOption index label xPos yPos selectedIndex =
  if label == "-" then blank else drawMenuOption index (map toUpper label) xPos yPos selectedIndex
