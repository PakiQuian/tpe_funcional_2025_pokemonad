module Client.Screens.PokemonScreen (drawPokemonScreen) where

import Data.List (intercalate)
import Client.Drawing (drawLogo, drawTextWithShadow, pokemonBlue, pokemonYellow)
import Pokemonad.Core.Pokemon (Pokemon (..), getPokemonById)
import Pokemonad.Core.Types (PokemonId (..), PokemonType (..), Stats (..))
import Graphics.Gloss
  ( Picture,
    circleSolid,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    scale,
    text,
    translate,
    white,
  )

drawPokemonScreen :: Picture -> Picture -> PokemonId -> Maybe Picture -> Picture
drawPokemonScreen menuBgImage logoImage selectedId maybeSprite =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      drawDetailBox selectedId maybeSprite,
      drawInstructions
    ]

drawDetailBox :: PokemonId -> Maybe Picture -> Picture
drawDetailBox selectedId maybeSprite =
  translate 0 (-50) $
    pictures
      [ color white $ rectangleSolid 720 440,
        color pokemonBlue $ rectangleSolid 700 420,
        color white $ translate 0 0 $ rectangleSolid 2 400,
        translate (-170) 0 $ drawPokemonInfo selectedId,
        translate 170 0 $ drawPokemonDisplay maybeSprite
      ]

drawPokemonInfo :: PokemonId -> Picture
drawPokemonInfo selectedId =
  case getPokemonById selectedId of
    Nothing ->
      scale 0.2 0.2 $ color white $ text "Pokemon Not Found"
    Just p ->
      pictures
        [ translate (-160) 150 $ scale 0.25 0.25 $ color pokemonYellow $ text (pokemonName p),
          translate 80 150 $ scale 0.25 0.25 $ color white $ text ("#" ++ formatNumber (unPokemonId selectedId)),
          translate (-160) 110 $ scale 0.15 0.15 $ color white $ text "TYPE:",
          translate (-90) 110 $ scale 0.15 0.15 $ color pokemonYellow $ text (showTypes (pokemonTypes p)),
          translate (-160) 60 $ scale 0.12 0.12 $ color white $ text (take 35 (pokemonDescription p)),
          translate (-160) 40 $ scale 0.12 0.12 $ color white $ text (drop 35 (pokemonDescription p)),
          drawStatsTable (pokemonStats p)
        ]

drawStatsTable :: Stats -> Picture
drawStatsTable s =
  translate (-160) (-20) $
    pictures
      [ drawStatRow 0 "HP" (statsHp s),
        drawStatRow 1 "Attack" (statsAttack s),
        drawStatRow 2 "Defense" (statsDefense s),
        translate 160 0 $
          pictures
            [ drawStatRow 0 "Sp. Atk" (statsSpecialAttack s),
              drawStatRow 1 "Sp. Def" (statsSpecialDefense s),
              drawStatRow 2 "Speed" (statsSpeed s)
            ]
      ]

drawStatRow :: Int -> String -> Int -> Picture
drawStatRow row label val =
  let yPos = fromIntegral (row * (-45))
      barWidth = fromIntegral val * 0.6
      maxBarWidth = 150
      statColor
        | val < 50 = makeColorI 255 80 80 255
        | val < 80 = makeColorI 255 140 0 255
        | val < 110 = makeColorI 255 200 0 255
        | val < 150 = makeColorI 0 255 255 255
        | otherwise = makeColorI 180 60 255 255
   in pictures
        [ translate 0 yPos $
            scale 0.12 0.12 $
              color white $
                text label,
          translate 80 yPos $
            scale 0.12 0.12 $
              color statColor $
                text (show val),
          translate 0 (yPos - 15) $
            pictures
              [ translate (maxBarWidth / 2) 0 $
                  color (makeColorI 50 50 50 255) $
                    rectangleSolid maxBarWidth 6,
                translate (barWidth / 2) 0 $
                  color statColor $
                    rectangleSolid barWidth 6
              ]
        ]

drawPokemonDisplay :: Maybe Picture -> Picture
drawPokemonDisplay Nothing =
  pictures
    [ color (makeColorI 0 0 0 100) $ circleSolid 70,
      color white $ scale 0.2 0.2 $ translate (-20) (-20) $ text "?"
    ]
drawPokemonDisplay (Just pic) = scale 3.5 3.5 pic

formatNumber :: Int -> String
formatNumber n
  | n < 10 = "00" ++ show n
  | n < 100 = "0" ++ show n
  | otherwise = show n

showTypes :: [PokemonType] -> String
showTypes ts = intercalate " / " (map show ts)

drawInstructions :: Picture
drawInstructions =
  pictures
    [ translate 0 (-320) $
        color (makeColorI 0 0 0 200) $
          rectangleSolid 1280 50,
      drawTextWithShadow
        "BACKSPACE: Return to Pokedex"
        0.15
        (-327)
        white
    ]
