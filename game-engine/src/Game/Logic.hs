module Game.Logic where

import Game.Move (Move (..), MoveCategory (..))
import Game.Pokemon (Pokemon (..), PokemonType (..), Stats (..))

-- Tabla de Efectividad (Simplificada para empezar)
-- Devuelve multiplicador: 2.0 (Super Effective), 0.5 (Not Very), 1.0 (Normal), 0.0 (Immune)
getMultiplier :: PokemonType -> PokemonType -> Float
getMultiplier Fire Grass = 2.0
getMultiplier Fire Water = 0.5
getMultiplier Water Fire = 2.0
getMultiplier Water Grass = 0.5
getMultiplier Grass Water = 2.0
getMultiplier Grass Fire = 0.5
getMultiplier Electric Water = 2.0
getMultiplier Electric Ground = 0.0
getMultiplier Ground Electric = 2.0
getMultiplier Flying Grass = 2.0
getMultiplier Flying Electric = 0.5
getMultiplier Psychic Fighting = 2.0
-- ... (Agregar más tipos según necesidad)
getMultiplier _ _ = 1.0

-- Cálculo de Daño Gen 1/3 Estándar
-- Damage = ((((2 * Level / 5 + 2) * Attack * Power / Defense) / 50) + 2) * Modifier
calculateDamage :: Int -> Int -> Int -> Int -> Float -> Int
calculateDamage level attack defense power typeEffectiveness =
  let base = fromIntegral ((2 * level `div` 5 + 2) * power * attack `div` defense) / 50.0 + 2.0 :: Float
      final = base * typeEffectiveness
   in floor final