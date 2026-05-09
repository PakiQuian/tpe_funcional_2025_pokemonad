module Client.Screens.PokedexScreen (drawPokedexScreen) where

import Client.Drawing (drawCenteredText, drawLogo, drawTextWithShadow, pokemonBlue, pokemonYellow)
import Pokemonad.Core.Pokemon (Pokemon (..), allPokemon)
import Pokemonad.Core.Types (PokemonId (..))
import Graphics.Gloss
  ( Picture,
    blank,
    circleSolid,
    color,
    makeColorI,
    pictures,
    polygon,
    rectangleSolid,
    scale,
    text,
    translate,
    white,
  )

drawPokedexScreen :: Picture -> Picture -> PokemonId -> Maybe Picture -> Picture
drawPokedexScreen menuBgImage logoImage selectedId currentSprite =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      drawPokedexBox selectedId currentSprite,
      drawInstructions
    ]

drawPokedexBox :: PokemonId -> Maybe Picture -> Picture
drawPokedexBox selectedId maybeSprite =
  translate 0 (-50) $
    pictures
      [ color white $ rectangleSolid 720 440,
        color pokemonBlue $ rectangleSolid 700 420,
        color white $ translate 0 0 $ rectangleSolid 2 400,
        translate (-170) 0 $ drawScrollingList selectedId,
        translate 170 0 $ drawPokemonDisplay maybeSprite
      ]

drawScrollingList :: PokemonId -> Picture
drawScrollingList selectedId = pictures (zipWith drawEntry visiblePokemon [0 ..])
  where
    maxItems = 10
    selIndex = max 0 (unPokemonId selectedId - 1)
    scrollOffset = max 0 (selIndex - (maxItems `div` 2))
    visiblePokemon = take maxItems $ drop scrollOffset allPokemon

    drawEntry :: Pokemon -> Int -> Picture
    drawEntry p offset =
      let isSelected = pokemonId p == selectedId
          yPos = 160 - (fromIntegral offset * 35)
          nameColor = if isSelected then white else makeColorI 180 180 180 255
          numColor = pokemonYellow
          txtNum =
            translate (-140) yPos $
              scale 0.12 0.12 $
                color numColor $
                  text ("#" ++ formatNumber (unPokemonId (pokemonId p)))
          txtName =
            translate (-90) yPos $
              scale 0.12 0.12 $
                color nameColor $
                  text (pokemonName p)
          cursor =
            if isSelected
              then
                translate (-155) (yPos + 1) $
                  color pokemonYellow $
                    polygon [(0, 0), (0, 10), (8, 5)]
              else blank
       in pictures [cursor, txtNum, txtName]

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

drawInstructions :: Picture
drawInstructions =
  pictures
    [ translate 0 (-320) $
        color (makeColorI 0 0 0 200) $
          rectangleSolid 1280 50,
      drawTextWithShadow
        "UP/DOWN: Navigate   |   ENTER: Detail   |   BACKSPACE: Menu"
        0.15
        (-327)
        white
    ]
