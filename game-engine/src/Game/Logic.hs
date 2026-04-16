module Game.Logic where

import Game.Move (Move (..), MoveCategory (..))
import Game.Pokemon (Pokemon (..))
import Game.Types (PokemonType (..), Stats (..), Status (..))
import System.Random (StdGen, randomR)

-- Tabla de Efectividad (Simplificada para empezar)
-- Devuelve multiplicador: 2.0 (Super Effective), 0.5 (Not Very), 1.0 (Normal), 0.0 (Immune)
getTypeEffectiveness :: PokemonType -> PokemonType -> Float
getTypeEffectiveness Fire Grass = 2.0
getTypeEffectiveness Fire Water = 0.5
getTypeEffectiveness Water Fire = 2.0
getTypeEffectiveness Water Grass = 0.5
getTypeEffectiveness Grass Water = 2.0
getTypeEffectiveness Grass Fire = 0.5
getTypeEffectiveness Electric Water = 2.0
getTypeEffectiveness Electric Ground = 0.0
getTypeEffectiveness Ground Electric = 2.0
getTypeEffectiveness Flying Grass = 2.0
getTypeEffectiveness Flying Electric = 0.5
getTypeEffectiveness Psychic Fighting = 2.0
-- ... (Agregar más tipos según necesidad)
getTypeEffectiveness _ _ = 1.0

-- Cálculo de Daño Gen 1/3 Estándar
-- Damage = ((((2 * Level / 5 + 2) * Attack * Power / Defense) / 50) + 2) * Modifier
calculateDamage :: Int -> Int -> Int -> Int -> Float -> Int
calculateDamage level attack defense power typeEffectiveness =
  let base = fromIntegral ((2 * level `div` 5 + 2) * power * attack `div` defense) / 50.0 + 2.0 :: Float
      final = base * typeEffectiveness
   in floor final

-- ==========================================
-- RESOLUCIÓN DE TURNO
-- ==========================================

-- Chequea si un pokemon está en estado que le impide atacar
canAttack :: Status -> Bool
canAttack Fainted = False
canAttack Asleep = False -- TODO: Implementar contador de sueño
canAttack Frozen = False -- TODO: Implementar 20% de descongelamiento
canAttack _ = True

-- Chequea si un ataque golpea (basado en precisión del movimiento)
doesMoveHit :: StdGen -> Int -> (Bool, StdGen) -- rng, mAccuracy
doesMoveHit rng accuracy =
  let (roll, nextRng) = randomR (1, 100) rng
   in (roll <= accuracy, nextRng)

-- Calcula el daño completo entre dos pokémon
resolveDamage ::
  Int -> -- nivel del atacante
  Int -> -- ataque del atacante (o sp. ataque si es Special)
  Int -> -- defensa del defensor (o sp. defensa si es Special)
  Move -> -- movimiento
  PokemonType -> -- tipo del defensor (principal)
  Int -- daño final
resolveDamage level atkStat defStat move defType =
  if mPower move == 0
    then 0 -- Movimiento de estado no hace daño
    else
      let typeEff = getTypeEffectiveness (mType move) defType
          baseDmg = calculateDamage level atkStat defStat (mPower move) typeEff
       in max 1 baseDmg -- Mínimo 1 de daño si golpea

-- Obtiene el stat apropiado basado en la categoría del movimiento
getAttackStat :: Move -> Stats -> Int
getAttackStat move stats =
  case mCategory move of
    Physical -> attack stats
    Special -> specialAttack stats
    Status -> 0 -- No importa para estados

getDefenseStat :: Move -> Stats -> Int
getDefenseStat move stats =
  case mCategory move of
    Physical -> defense stats
    Special -> specialDefense stats
    Status -> 0 -- No importa para estados
