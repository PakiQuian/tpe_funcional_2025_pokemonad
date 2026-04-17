module Screens.BattleEndScreen (drawBattleEndScreen) where

import Engine.Common (pokemonBlue)
import Game.Battle (BattleState, Winner (..))
import Game.Trainer (Trainer)
import Graphics.Gloss
  ( Picture,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    scale,
    text,
    translate,
    white,
  )
import qualified Data.Map as Map

-- Pantalla final simplificada: fondo + caja azul con resultado.
drawBattleEndScreen :: Picture -> Winner -> Maybe BattleState -> Maybe Trainer -> Map.Map Int Picture -> Map.Map Int Picture -> Map.Map Int Picture -> Picture
drawBattleEndScreen resultBg winner _maybeState _maybeTrainer _trainerSprites _pokemonFrontSprites _pokemonBackSprites =
  let (titleText, titleOffset) =
        case winner of
          PlayerWon -> ("YOU WON", -120)
          EnemyWon -> ("YOU LOST", -130)
      (subtitleText, subtitleOffset) =
        case winner of
          PlayerWon -> ("The battle is over.", -130)
          EnemyWon -> ("Your team was defeated.", -155)
   in pictures
        [ resultBg,
          translate 0 40 $
            pictures
              [ color white $ rectangleSolid 620 250,
                color pokemonBlue $ rectangleSolid 600 230,
                translate titleOffset 40 $ scale 0.35 0.35 $ color white $ text titleText,
                translate subtitleOffset (-40) $ scale 0.18 0.18 $ color (makeColorI 220 220 220 255) $ text subtitleText
              ]
        ]
