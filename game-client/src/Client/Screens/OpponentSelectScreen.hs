module Client.Screens.OpponentSelectScreen (drawOpponentSelectScreen) where

import Client.Drawing (cursorYellowColor, drawCenteredText, drawLogo, drawTextWithShadow, panelBlueColor)
import qualified Data.Map as Map
import Graphics.Gloss
  ( Picture,
    blank,
    circleSolid,
    color,
    makeColorI,
    pictures,
    polygon,
    rectangleSolid,
    red,
    scale,
    text,
    translate,
    white,
  )
import Pokemonad.Core.Trainer (AIDifficulty (..), Trainer (..), allTrainers)
import Pokemonad.Core.Types (PokemonId (..), TrainerId (..))

drawOpponentSelectScreen :: Picture -> Picture -> Int -> Map.Map PokemonId Picture -> Map.Map TrainerId Picture -> Picture
drawOpponentSelectScreen menuBgImage logoImage selectedIndex pokemonSpriteMap trainerSpriteMap =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      drawOpponentBox selectedIndex pokemonSpriteMap trainerSpriteMap,
      drawInstructions
    ]

drawOpponentBox :: Int -> Map.Map PokemonId Picture -> Map.Map TrainerId Picture -> Picture
drawOpponentBox selectedIndex pokemonSpriteMap trainerSpriteMap =
  translate 0 (-50) $
    pictures
      [ color white $ rectangleSolid 720 440,
        color panelBlueColor $ rectangleSolid 700 420,
        color white $ translate 0 (-20) $ rectangleSolid 2 350,
        translate 0 170 $ drawTextWithShadow "CHOOSE YOUR OPPONENT" 0.18 0 white,
        translate (-170) 0 $ drawTrainerList selectedIndex,
        translate 170 0 $ drawTrainerPreview (allTrainers !! selectedIndex) pokemonSpriteMap trainerSpriteMap
      ]

drawTrainerList :: Int -> Picture
drawTrainerList selectedIndex = pictures $ zipWith drawTrainerEntry visibleTrainers [0 ..]
  where
    maxItems = 8
    scrollOffset = max 0 (selectedIndex - (maxItems `div` 2))
    visibleTrainers = take maxItems $ drop scrollOffset allTrainers

    drawTrainerEntry :: Trainer -> Int -> Picture
    drawTrainerEntry trainer offset =
      let realIndex = scrollOffset + offset
          isSelected = realIndex == selectedIndex
          yPos = 120 - (fromIntegral offset * 45)
          nameColor = if isSelected then white else makeColorI 180 180 180 255
          txtName =
            translate (-140) yPos $
              scale 0.15 0.15 $
                color nameColor $
                  text (trainerName trainer)
          cursor =
            if isSelected
              then
                translate (-160) (yPos + 2) $
                  color cursorYellowColor $
                    polygon [(0, 0), (0, 12), (10, 6)]
              else blank
          stars =
            translate (-140) (yPos - 15) $
              scale 0.12 0.12 $
                color cursorYellowColor $
                  text (difficultyStars (trainerDifficulty trainer))
       in pictures [cursor, txtName, stars]

difficultyStars :: AIDifficulty -> String
difficultyStars DifficultyEasy = "**"
difficultyStars DifficultyMedium = "***"
difficultyStars DifficultyHard = "*****"
difficultyStars DifficultyExtreme = "*******"

drawTrainerPreview :: Trainer -> Map.Map PokemonId Picture -> Map.Map TrainerId Picture -> Picture
drawTrainerPreview trainer pokemonSpriteMap trainerSpriteMap =
  let isRandom = null (trainerTeam trainer)
   in pictures
        [ if isRandom
            then translate 0 0 $ drawTrainerSprite (trainerId trainer) trainerSpriteMap
            else translate 0 70 $ drawTrainerSprite (trainerId trainer) trainerSpriteMap,
          if isRandom
            then translate 0 (-100) $ drawCenteredText (trainerName trainer) 0.18 0 white
            else translate 0 0 $ drawCenteredText (trainerName trainer) 0.15 0 white,
          if isRandom
            then blank
            else translate 0 (-110) $ drawMiniTeamGrid (trainerTeam trainer) pokemonSpriteMap
        ]

drawTrainerSprite :: TrainerId -> Map.Map TrainerId Picture -> Picture
drawTrainerSprite tid spriteMap =
  case Map.lookup tid spriteMap of
    Just pic -> scale 2.5 2.5 pic
    Nothing ->
      pictures
        [ color (makeColorI 255 255 255 50) $ circleSolid 60,
          scale 0.1 0.1 $ translate (-100) 0 $ color white $ text "NO IMG"
        ]

drawMiniTeamGrid :: [PokemonId] -> Map.Map PokemonId Picture -> Picture
drawMiniTeamGrid teamIds spriteMap =
  pictures
    [ drawCenteredText "TEAM PREVIEW" 0.10 90 cursorYellowColor,
      pictures $ zipWith (drawMiniSlot spriteMap) [0 ..] teamIds
    ]

drawMiniSlot :: Map.Map PokemonId Picture -> Int -> PokemonId -> Picture
drawMiniSlot spriteMap index pid =
  let col = index `mod` 3
      row = index `div` 3
      xPos = -70 + (fromIntegral col * 70)
      yPos = 30 - (fromIntegral row * 70)
   in translate xPos yPos $ case Map.lookup pid spriteMap of
        Nothing -> scale 0.1 0.1 $ color red $ text "?"
        Just pic -> scale 0.7 0.7 pic

drawInstructions :: Picture
drawInstructions =
  pictures
    [ translate 0 (-320) $ color (makeColorI 0 0 0 200) $ rectangleSolid 1280 50,
      drawTextWithShadow "ENTER: Start Battle | BACKSPACE: Back" 0.15 (-327) white
    ]
