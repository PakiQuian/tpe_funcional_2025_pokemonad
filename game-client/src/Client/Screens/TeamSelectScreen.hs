module Client.Screens.TeamSelectScreen (drawTeamSelectScreen) where

import Client.Drawing (cursorYellowColor, drawLogo, drawTextWithShadow, panelBlueColor)
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
import Pokemonad.Core.Pokemon (Pokemon (..), allPokemon)
import Pokemonad.Core.Types (PokemonId (..))

drawTeamSelectScreen :: Picture -> Picture -> PokemonId -> [PokemonId] -> Map.Map PokemonId Picture -> Picture
drawTeamSelectScreen menuBgImage logoImage selectedId team spriteMap =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      translate (-320) (-50) $ drawSelectionBox selectedId team spriteMap,
      translate 320 (-50) $ drawTeamGrid team spriteMap,
      drawInstructions
    ]

drawSelectionBox :: PokemonId -> [PokemonId] -> Map.Map PokemonId Picture -> Picture
drawSelectionBox selectedId team spriteMap =
  pictures
    [ color white $ rectangleSolid 520 440,
      color panelBlueColor $ rectangleSolid 500 420,
      color white $ translate 0 0 $ rectangleSolid 2 400,
      translate (-120) 0 $ drawScrollingList selectedId team,
      translate 120 0 $ drawPokemonDisplay selectedId spriteMap
    ]

drawTeamGrid :: [PokemonId] -> Map.Map PokemonId Picture -> Picture
drawTeamGrid team spriteMap =
  pictures
    [ color white $ rectangleSolid 520 440,
      color panelBlueColor $ rectangleSolid 500 420,
      translate 0 170 $ drawTextWithShadow ("TEAM (" ++ show (length team) ++ "/6)") 0.2 0 white,
      drawSlots team spriteMap
    ]

drawSlots :: [PokemonId] -> Map.Map PokemonId Picture -> Picture
drawSlots team spriteMap =
  pictures
    [ drawTeamSlot spriteMap team 0 (-110) 100,
      drawTeamSlot spriteMap team 1 110 100,
      drawTeamSlot spriteMap team 2 (-110) (-20),
      drawTeamSlot spriteMap team 3 110 (-20),
      drawTeamSlot spriteMap team 4 (-110) (-140),
      drawTeamSlot spriteMap team 5 110 (-140)
    ]

drawTeamSlot :: Map.Map PokemonId Picture -> [PokemonId] -> Int -> Float -> Float -> Picture
drawTeamSlot spriteMap team index xPos yPos =
  translate xPos yPos $
    pictures
      [ color white $ rectangleSolid 180 100,
        color (makeColorI 0 30 80 255) $ rectangleSolid 170 90,
        if index < length team
          then drawSlotContent spriteMap (team !! index)
          else drawEmptySlot index
      ]

drawSlotContent :: Map.Map PokemonId Picture -> PokemonId -> Picture
drawSlotContent spriteMap pid =
  case Map.lookup pid spriteMap of
    Just pic -> scale 1.5 1.5 pic
    Nothing -> color red $ circleSolid 20

drawEmptySlot :: Int -> Picture
drawEmptySlot index =
  color (makeColorI 255 255 255 50) $
    scale 0.3 0.3 $
      translate (-45) (-45) $
        text (show (index + 1))

drawScrollingList :: PokemonId -> [PokemonId] -> Picture
drawScrollingList selectedId team = pictures (zipWith drawEntry visiblePokemon [0 ..])
  where
    maxItems = 10
    selIndex = max 0 (unPokemonId selectedId - 1)
    scrollOffset = max 0 (selIndex - (maxItems `div` 2))
    visiblePokemon = take maxItems $ drop scrollOffset allPokemon

    drawEntry :: Pokemon -> Int -> Picture
    drawEntry p offset =
      let isSelected = pokemonId p == selectedId
          isInTeam = pokemonId p `elem` team
          yPos = 160 - (fromIntegral offset * 35)
          nameColor
            | isInTeam = makeColorI 100 255 100 255
            | isSelected = white
            | otherwise = makeColorI 180 180 180 255
          txtNum = translate (-110) yPos $ scale 0.12 0.12 $ color cursorYellowColor $ text ("#" ++ formatNumber (unPokemonId (pokemonId p)))
          txtName = translate (-60) yPos $ scale 0.12 0.12 $ color nameColor $ text (take 10 (pokemonName p))
          cursor =
            if isSelected
              then translate (-125) (yPos + 2) $ color cursorYellowColor $ polygon [(0, 0), (0, 10), (8, 5)]
              else blank
       in pictures [cursor, txtNum, txtName]

drawPokemonDisplay :: PokemonId -> Map.Map PokemonId Picture -> Picture
drawPokemonDisplay selectedId spriteMap =
  case Map.lookup selectedId spriteMap of
    Nothing -> scale 0.2 0.2 $ color white $ text "?"
    Just pic -> scale 2.5 2.5 pic

drawInstructions :: Picture
drawInstructions =
  pictures
    [ translate 0 (-320) $
        color (makeColorI 0 0 0 200) $
          rectangleSolid 1280 50,
      drawTextWithShadow
        "ENTER: Add / Continue | BACKSPACE: Remove / Back | R: Random Team"
        0.15
        (-327)
        white
    ]

formatNumber :: Int -> String
formatNumber n
  | n < 10 = "00" ++ show n
  | n < 100 = "0" ++ show n
  | otherwise = show n
