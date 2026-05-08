module Screens.AISimulatorScreen (drawAISimulatorScreen) where

import Engine.Common (drawLogo, drawTextWithShadow, pokemonBlue, pokemonYellow)
import Engine.GameState (GameState (..))
import Graphics.Gloss
  ( Picture,
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

drawAISimulatorScreen :: Picture -> Picture -> GameState -> Picture
drawAISimulatorScreen menuBgImage logoImage gs =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      drawMainPanel gs,
      drawFooter (simulatorTraining gs)
    ]

drawMainPanel :: GameState -> Picture
drawMainPanel gs =
  let training = simulatorTraining gs
      logs = take 12 (simulatorLogs gs)
   in pictures
        [ color white $ rectangleSolid 980 470,
          color pokemonBlue $ rectangleSolid 960 450,
          drawTextWithShadow "AI TRAINING SIMULATOR" 0.2 175 white,
          drawTextWithShadow "Offline self-play  |  ENTER runs 100 epochs" 0.10 140 (makeColorI 200 220 255 255),
          drawRunButton training,
          drawEpochCounter (simulatorTotalEpochs gs),
          drawStatusBox training (simulatorStatus gs),
          drawLogsBox logs
        ]

drawRunButton :: Bool -> Picture
drawRunButton isTraining =
  let bgColor = if isTraining then makeColorI 50 50 50 160 else makeColorI 0 0 0 140
      borderColor = if isTraining then makeColorI 100 100 100 200 else makeColorI 170 215 255 255
      label = if isTraining then "TRAINING IN PROGRESS..." else "START TRAINING  (100 epochs)"
      labelColor = if isTraining then makeColorI 140 140 140 255 else white
      labelX = -(fromIntegral (length label) * 78 * 0.13 / 2)
   in translate 0 88 $
        pictures
          [ color bgColor $ rectangleSolid 500 46,
            color borderColor $ rectangleWire 500 46,
            if isTraining
              then blank
              else translate (labelX - 22) (-9) $ color pokemonYellow $ polygon [(0, 0), (0, 14), (11, 7)],
            translate (labelX + 2) (-9) $
              scale 0.13 0.13 $
                color labelColor $
                  text label
          ]

drawEpochCounter :: Int -> Picture
drawEpochCounter totalEpochs =
  translate (-420) 42 $
    scale 0.12 0.12 $
      color (makeColorI 200 230 255 255) $
        text ("Total epochs trained: " ++ show totalEpochs)

drawStatusBox :: Bool -> String -> Picture
drawStatusBox isTraining statusLine =
  let statusColor = if isTraining then makeColorI 255 220 80 255 else white
      labelX = -(fromIntegral (length clipped) * 78 * 0.11 / 2)
      clipped = clipText 80 statusLine
   in translate 0 5 $
        pictures
          [ color (makeColorI 0 0 0 155) $ rectangleSolid 880 36,
            color (makeColorI 170 215 255 255) $ rectangleWire 880 36,
            translate labelX (-7) $
              scale 0.11 0.11 $
                color statusColor $
                  text clipped
          ]

drawLogsBox :: [String] -> Picture
drawLogsBox logs =
  translate 0 (-120) $
    pictures
      [ color (makeColorI 0 0 0 150) $ rectangleSolid 860 200,
        color (makeColorI 170 215 255 255) $ rectangleWire 860 200,
        translate (-385) 81 $
          scale 0.12 0.12 $
            color (makeColorI 200 230 255 255) $
              text "Epoch logs (newest first)",
        pictures (zipWith drawLogLine [0 ..] logs)
      ]

drawLogLine :: Int -> String -> Picture
drawLogLine idx msg =
  let y = 66 - fromIntegral idx * 14
   in translate (-390) y $
        scale 0.10 0.10 $
          color white $
            text (clipText 126 msg)

drawFooter :: Bool -> Picture
drawFooter isTraining =
  pictures
    [ translate 0 (-320) $
        color (makeColorI 0 0 0 200) $
          rectangleSolid 1280 52,
      drawTextWithShadow footerText 0.12 (-327) white
    ]
  where
    footerText =
      if isTraining
        then "Training in progress... please wait"
        else "ENTER: Run 100 epochs  |  ESC: Back to menu"

clipText :: Int -> String -> String
clipText maxLen s
  | length s <= maxLen = s
  | maxLen <= 1 = take maxLen s
  | otherwise = take (maxLen - 1) s ++ "..."
