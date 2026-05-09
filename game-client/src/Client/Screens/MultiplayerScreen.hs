module Client.Screens.MultiplayerScreen (drawMultiplayerScreen) where

import Client.Drawing
  ( cursorYellowColor,
    drawLogo,
    drawTextWithShadow,
    footerBgColor,
    panelBlueColor,
    panelBorderColor,
    subtitleBlueColor,
  )
import Client.Types (MultiplayerState (..), NetSubState (..))
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

-- | P2P screen aligned with Menu / TeamSelect: background, logo, panel, and typography.
drawMultiplayerScreen :: Picture -> Picture -> MultiplayerState -> NetSubState -> Picture
drawMultiplayerScreen menuBgImage logoImage ms netSt =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      translate 0 0 $ drawMainPanel ms netSt,
      drawFooter
    ]

-- Main panel (same white border + blue interior pattern as the menu).
drawMainPanel :: MultiplayerState -> NetSubState -> Picture
drawMainPanel ms netSt =
  let row = mpCursor ms
      hostStr = mpHost ms
      portStr = mpPort ms
      netLine = netStatusLine netSt
      errLine = mpError ms
   in pictures
        [ color white $ rectangleSolid 720 320,
          color panelBlueColor $ rectangleSolid 700 300,
          drawTextWithShadow "P2P MULTIPLAYER" 0.2 95 white,
          drawTextWithShadow "TCP - Host or Client" 0.12 55 subtitleBlueColor,
          drawFieldRow row 0 10 "HOST" hostStr,
          drawFieldRow row 1 (-30) "PORT" portStr,
          drawActionRow row 2 (-70) "LISTEN ON THIS PORT (HOST)",
          drawActionRow row 3 (-110) "CONNECT TO PROVIDED HOST",
          drawNetStatusBox netLine errLine
        ]

netStatusLine :: NetSubState -> String
netStatusLine netSt = case netSt of
  NetDisconnected -> "Status: disconnected"
  NetListening p -> "Status: listening on " ++ show p
  NetConnecting h p -> "Status: connecting to " ++ h ++ ":" ++ show p
  NetInLobby -> "Status: connected (lobby)"
  NetInBattle -> "Status: connected (battle)"

drawNetStatusBox :: String -> Maybe String -> Picture
drawNetStatusBox netLine maybeErr =
  let rawLine = case maybeErr of
        Just err -> "Error: " ++ err
        Nothing -> netLine
      line = clipText 64 rawLine
      lineColor = case maybeErr of
        Just _ -> makeColorI 255 210 210 255
        Nothing -> white
   in pictures
        [ translate 0 (-220) $ color (makeColorI 0 0 0 165) $ rectangleSolid 640 44,
          translate 0 (-220) $ color panelBorderColor $ rectangleWire 640 44,
          translate (-260) (-227) $
            scale 0.11 0.11 $
              color lineColor $
                text line
        ]

clipText :: Int -> String -> String
clipText maxLen s
  | length s <= maxLen = s
  | maxLen <= 1 = take maxLen s
  | otherwise = take (maxLen - 1) s ++ "…"

-- | Row with label + value (host / port).
drawFieldRow :: Int -> Int -> Float -> String -> String -> Picture
drawFieldRow currentRow idx yBase label val =
  let sel = currentRow == idx
      valColor = if sel then cursorYellowColor else makeColorI 195 195 195 255
      line =
        pictures
          [ translate (-260) yBase $
              scale 0.14 0.14 $
                color (makeColorI 160 200 255 255) $
                  text (label ++ ":"),
            translate (-120) yBase $
              scale 0.14 0.14 $
                color valColor $
                  text val
          ]
      cursor =
        if sel
          then translate (-295) (yBase + 5) $ color cursorYellowColor $ polygon [(0, 0), (0, 14), (11, 7)]
          else blank
   in pictures [cursor, line]

-- | Action row (descriptive text only).
drawActionRow :: Int -> Int -> Float -> String -> Picture
drawActionRow currentRow idx yBase label =
  let sel = currentRow == idx
      txtColor = if sel then white else makeColorI 165 165 165 255
      cursor =
        if sel
          then translate (-295) (yBase + 5) $ color cursorYellowColor $ polygon [(0, 0), (0, 14), (11, 7)]
          else blank
      txt =
        translate (-260) yBase $
          scale 0.13 0.13 $
            color txtColor $
              text label
   in pictures [cursor, txt]

-- | Bottom footer similar to TeamSelect (dark semi-transparent background + shadowed text).
drawFooter :: Picture
drawFooter =
  pictures
    [ translate 0 (-320) $
        color footerBgColor $
          rectangleSolid 1280 52,
      drawTextWithShadow
        "ENTER: Confirm row  |  UP/DOWN: Move  |  ESC: Back to menu"
        0.13
        (-327)
        white
    ]
