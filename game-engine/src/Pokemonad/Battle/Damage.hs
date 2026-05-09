module Pokemonad.Battle.Damage
  ( getTypeEffectiveness,
    canAttack,
    doesMoveHit,
    resolveDamage,
    getAttackStat,
    getDefenseStat,
  )
where

import Pokemonad.Core.Move (Move (..), MoveCategory (..))
import Pokemonad.Core.Types (HP (..), Level (..), PokemonType (..), Stats (..), Status (..))
import System.Random (StdGen, randomR)

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
getTypeEffectiveness _ _ = 1.0

calculateDamage :: Int -> Int -> Int -> Int -> Float -> Int
calculateDamage level attack defense power typeEffectiveness =
  let base = fromIntegral ((2 * level `div` 5 + 2) * power * attack `div` defense) / 50.0 + 2.0 :: Float
   in floor (base * typeEffectiveness)

canAttack :: Status -> Bool
canAttack Fainted = False
canAttack Asleep = False
canAttack Frozen = False
canAttack _ = True

doesMoveHit :: StdGen -> Int -> (Bool, StdGen)
doesMoveHit rng accuracy =
  let (roll, nextRng) = randomR (1, 100) rng
   in (roll <= accuracy, nextRng)

resolveDamage :: Level -> Int -> Int -> Move -> PokemonType -> HP
resolveDamage (Level level) atkStat defStat move defType =
  if movePower move == 0
    then HP 0
    else
      let typeEff = getTypeEffectiveness (moveType move) defType
          baseDmg = calculateDamage level atkStat defStat (movePower move) typeEff
       in HP (max 1 baseDmg)

getAttackStat :: Move -> Stats -> Int
getAttackStat move stats =
  case moveCategory move of
    Physical -> statsAttack stats
    Special -> statsSpecialAttack stats
    Status -> 0

getDefenseStat :: Move -> Stats -> Int
getDefenseStat move stats =
  case moveCategory move of
    Physical -> statsDefense stats
    Special -> statsSpecialDefense stats
    Status -> 0
