module Pokemonad.AI.Model
  ( FeatureVector,
    QWeights (..),
    defaultQWeights,
    candidateActions,
    extractFeatures,
    qValue,
    chooseActionGreedy,
    chooseActionEpsilon,
    difficultyExplorationRate,
  )
where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Pokemonad.Battle.Damage (getAttackStat, getDefenseStat, getTypeEffectiveness, resolveDamage)
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
  )
import Pokemonad.Core.Move (Move (..))
import Pokemonad.Core.Pokemon (Pokemon (..))
import Pokemonad.Core.Trainer (AIDifficulty (..))
import Pokemonad.Core.Types (HP (..), Stats (..), Status (..))
import System.Random (StdGen, randomR)

type FeatureVector = [Float]

data QWeights = QWeights
  { weightsBias :: Float,
    weightsCoefficients :: [Float]
  }
  deriving (Show, Eq)

defaultQWeights :: QWeights
defaultQWeights =
  QWeights
    { weightsBias = 0.0,
      weightsCoefficients =
        [ 0.15, -- self hp ratio
          -0.10, -- enemy hp ratio
          0.75, -- expected damage ratio
          0.55, -- type effectiveness
          0.25, -- speed advantage
          0.80, -- estimated KO chance
          0.45, -- defensive switch gain
          0.35, -- offensive switch gain
          0.22, -- move action prior
          -0.35 -- switch action prior
        ]
    }

candidateActions :: BattleState -> [BattleAction]
candidateActions bState =
  let moveCount = length (battlePokemonMoves (enemyActive bState))
      moveActions = [ActionMove i | i <- [0 .. moveCount - 1]]
      availableSwitches =
        [ ActionSwitch benchIdx
        | (benchIdx, bp) <- zip [0 ..] (enemyBench bState),
          battlePokemonStatus bp /= Fainted,
          shouldConsiderSwitch bState benchIdx bp
        ]
   in case phase bState of
        BattleEnded _ -> []
        _ -> moveActions ++ availableSwitches

extractFeatures :: BattleState -> BattleAction -> FeatureVector
extractFeatures bState action =
  [ selfHpRatio,
    opponentHpRatio,
    expectedDamageRatio,
    normalizedTypeEffectiveness,
    speedAdvantage,
    koEstimate,
    switchDefensiveGain,
    switchOffensiveGain,
    isMoveAction,
    isSwitchAction
  ]
  where
    selfActive = enemyActive bState
    opponentActive = playerActive bState
    selfHpRatio = safeRatio (unHP (battlePokemonHp selfActive)) (unHP (battlePokemonMaxHp selfActive))
    opponentHpRatio = safeRatio (unHP (battlePokemonHp opponentActive)) (unHP (battlePokemonMaxHp opponentActive))
    speedAdvantage =
      normalizeSigned $
        fromIntegral (statsSpeed (pokemonStats (battlePokemonBase selfActive)) - statsSpeed (pokemonStats (battlePokemonBase opponentActive))) / 100.0
    (expectedDamageRatio, normalizedTypeEffectiveness, koEstimate) = case action of
      ActionMove moveIdx ->
        case safeAt moveIdx (battlePokemonMoves selfActive) of
          Nothing -> (0.0, 0.5, 0.0)
          Just selectedMove ->
            let dmg = unHP (estimateDamage selfActive opponentActive selectedMove)
                maxOppHp = max 1 (unHP (battlePokemonMaxHp opponentActive))
                dmgRatio = clamp01 (fromIntegral dmg / fromIntegral maxOppHp)
                eff = moveTypeEffectiveness selectedMove opponentActive
                effNorm = normalizeTypeEffectiveness eff
                ko = if dmg >= unHP (battlePokemonHp opponentActive) then 1.0 else 0.0
             in (dmgRatio, effNorm, ko)
      ActionSwitch _ -> (0.0, 0.5, 0.0)
    (switchDefensiveGain, switchOffensiveGain) = case action of
      ActionSwitch benchIdx ->
        case safeAt benchIdx (enemyBench bState) of
          Nothing -> (0.0, 0.0)
          Just incoming ->
            ( estimateSwitchDefensiveGain selfActive incoming opponentActive,
              estimateSwitchOffensiveGain selfActive incoming opponentActive
            )
      _ -> (0.0, 0.0)
    isMoveAction = case action of ActionMove _ -> 1.0; _ -> 0.0
    isSwitchAction = case action of ActionSwitch _ -> 1.0; _ -> 0.0

qValue :: QWeights -> BattleState -> BattleAction -> Float
qValue weights bState action =
  weightsBias weights + sum (zipWith (*) (weightsCoefficients weights) (extractFeatures bState action))

chooseActionGreedy :: QWeights -> BattleState -> Maybe BattleAction
chooseActionGreedy weights bState =
  case candidateActions bState of
    [] -> Nothing
    actions -> Just (fst (maximumBy (comparing snd) [(a, qValue weights bState a) | a <- actions]))

chooseActionEpsilon :: StdGen -> Float -> QWeights -> BattleState -> (Maybe BattleAction, StdGen)
chooseActionEpsilon rng epsilon weights bState =
  case candidateActions bState of
    [] -> (Nothing, rng)
    actions ->
      let (roll, rng1) = randomR (0.0 :: Float, 1.0 :: Float) rng
       in if roll < clamp01 epsilon
            then
              let (idx, rng2) = randomR (0, length actions - 1) rng1
               in (Just (actions !! idx), rng2)
            else (chooseActionGreedy weights bState, rng1)

difficultyExplorationRate :: AIDifficulty -> Float
difficultyExplorationRate DifficultyEasy = 0.35
difficultyExplorationRate DifficultyMedium = 0.20
difficultyExplorationRate DifficultyHard = 0.08
difficultyExplorationRate DifficultyExtreme = 0.00

-- Internal helpers

safeAt :: Int -> [a] -> Maybe a
safeAt idx xs
  | idx < 0 || idx >= length xs = Nothing
  | otherwise = Just (xs !! idx)

safeRatio :: Int -> Int -> Float
safeRatio num den
  | den <= 0 = 0.0
  | otherwise = clamp01 (fromIntegral num / fromIntegral den)

clamp01 :: Float -> Float
clamp01 = max 0.0 . min 1.0

normalizeSigned :: Float -> Float
normalizeSigned = max (-1.0) . min 1.0

moveTypeEffectiveness :: Move -> BattlePokemon -> Float
moveTypeEffectiveness mv target =
  getTypeEffectiveness (moveType mv) (head (pokemonTypes (battlePokemonBase target)))

normalizeTypeEffectiveness :: Float -> Float
normalizeTypeEffectiveness eff
  | eff <= 0.0 = 0.0
  | eff <= 0.5 = 0.25
  | eff <= 1.0 = 0.5
  | otherwise = 1.0

estimateDamage :: BattlePokemon -> BattlePokemon -> Move -> HP
estimateDamage attacker defender selectedMove =
  let atkStat = getAttackStat selectedMove (pokemonStats (battlePokemonBase attacker))
      defStat = getDefenseStat selectedMove (pokemonStats (battlePokemonBase defender))
      defType = head (pokemonTypes (battlePokemonBase defender))
   in resolveDamage (battlePokemonLevel attacker) atkStat defStat selectedMove defType

estimateSwitchDefensiveGain :: BattlePokemon -> BattlePokemon -> BattlePokemon -> Float
estimateSwitchDefensiveGain current incoming opponent =
  normalizeSigned ((bestThreatAgainst current opponent - bestThreatAgainst incoming opponent) / 2.0)

estimateSwitchOffensiveGain :: BattlePokemon -> BattlePokemon -> BattlePokemon -> Float
estimateSwitchOffensiveGain current incoming opponent =
  normalizeSigned ((bestOffenseFor incoming opponent - bestOffenseFor current opponent) / 2.0)

bestOffenseFor :: BattlePokemon -> BattlePokemon -> Float
bestOffenseFor attacker defender =
  case battlePokemonMoves attacker of
    [] -> 1.0
    moves -> maximum (map (`moveTypeEffectiveness` defender) moves)

bestThreatAgainst :: BattlePokemon -> BattlePokemon -> Float
bestThreatAgainst defender attacker =
  case battlePokemonMoves attacker of
    [] -> 1.0
    moves -> maximum (map (`moveTypeEffectiveness` defender) moves)

shouldConsiderSwitch :: BattleState -> Int -> BattlePokemon -> Bool
shouldConsiderSwitch bState _benchIdx incoming =
  let current = enemyActive bState
      opponent = playerActive bState
      hpRatio = safeRatio (unHP (battlePokemonHp current)) (unHP (battlePokemonMaxHp current))
      defGain = estimateSwitchDefensiveGain current incoming opponent
      offGain = estimateSwitchOffensiveGain current incoming opponent
   in defGain > 0.20 || offGain > 0.25 || hpRatio < 0.35
