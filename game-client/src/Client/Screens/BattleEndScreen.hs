module Client.Screens.BattleEndScreen (drawBattleEndScreen) where

import Client.Drawing (pokemonBlue)
import Pokemonad.Battle.State (BattleState, Winner (..))
import Pokemonad.Core.Trainer (Trainer)
import Pokemonad.Core.Types (PokemonId (..), TrainerId (..))
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
drawBattleEndScreen :: Picture -> Winner -> Maybe BattleState -> Maybe Trainer -> Map.Map TrainerId Picture -> Map.Map PokemonId Picture -> Map.Map PokemonId Picture -> Picture
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
