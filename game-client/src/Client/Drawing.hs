module Client.Drawing
  ( -- named color constants
    panelBlueColor,
    cursorYellowColor,
    overlayDarkColor,
    overlayMedColor,
    footerBgColor,
    dimTextColor,
    hpBarGreenColor,
    hpBarYellowColor,
    hpBarRedColor,
    hpTrackColor,
    hudBgColor,
    subtitleBlueColor,
    panelBorderColor,
    -- drawing helpers
    drawLogo,
    drawCenteredText,
    drawTextWithShadow,
    loadPngSafe,
  )
where

import Data.Maybe (fromMaybe)
import Graphics.Gloss
  ( Color,
    Picture,
    black,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    red,
    scale,
    text,
    translate,
    white,
  )
import Graphics.Gloss.Juicy (loadJuicyPNG)

-- ---------------------------------------------------------------------------
-- Named color constants
-- ---------------------------------------------------------------------------

panelBlueColor :: Color
panelBlueColor = makeColorI 0 50 120 255

cursorYellowColor :: Color
cursorYellowColor = makeColorI 255 200 0 255

overlayDarkColor :: Color
overlayDarkColor = makeColorI 0 0 0 220

overlayMedColor :: Color
overlayMedColor = makeColorI 0 0 0 190

footerBgColor :: Color
footerBgColor = makeColorI 0 0 0 200

dimTextColor :: Color
dimTextColor = makeColorI 180 180 180 255

hpBarGreenColor :: Color
hpBarGreenColor = makeColorI 50 200 50 255

hpBarYellowColor :: Color
hpBarYellowColor = makeColorI 200 200 50 255

hpBarRedColor :: Color
hpBarRedColor = makeColorI 200 50 50 255

hpTrackColor :: Color
hpTrackColor = makeColorI 100 100 100 255

hudBgColor :: Color
hudBgColor = makeColorI 255 255 255 200

subtitleBlueColor :: Color
subtitleBlueColor = makeColorI 200 220 255 255

panelBorderColor :: Color
panelBorderColor = makeColorI 170 215 255 255

-- ---------------------------------------------------------------------------
-- Drawing helpers
-- ---------------------------------------------------------------------------

drawLogo :: Picture -> Picture
drawLogo logo = translate 0 250 $ scale 0.8 0.8 logo

drawTextWithShadow :: String -> Float -> Float -> Color -> Picture
drawTextWithShadow content scl yPos col =
  let charWidth = 78
      totalWidth = fromIntegral (length content) * charWidth * scl
      xOffset = -(totalWidth / 2)
      textPic c = translate xOffset yPos $ scale scl scl $ color c $ text content
   in pictures
        [ translate 2 (-2) (textPic black),
          textPic col
        ]

drawCenteredText :: String -> Float -> Float -> Color -> Picture
drawCenteredText content scl yPos col =
  let charWidth = 78
      xOffset = -((fromIntegral (length content) * charWidth * scl) / 2)
   in translate xOffset yPos $ scale scl scl $ color col $ text content

loadPngSafe :: FilePath -> IO Picture
loadPngSafe path = do
  maybePic <- loadJuicyPNG path
  case maybePic of
    Just pic -> return pic
    Nothing -> do
      putStrLn $ "WARNING: Could not load image: " ++ path
      return $
        pictures
          [ color red $ rectangleSolid 50 50,
            color white $ scale 0.1 0.1 $ translate (-200) 0 $ text "ERR"
          ]
