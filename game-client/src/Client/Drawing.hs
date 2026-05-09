module Client.Drawing
  ( pokemonBlue,
    pokemonYellow,
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

pokemonBlue :: Color
pokemonBlue = makeColorI 0 50 120 255

pokemonYellow :: Color
pokemonYellow = makeColorI 255 200 0 255

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
