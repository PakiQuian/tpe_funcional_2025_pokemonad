module Game.AIModel
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
import Game.Battle (BattleAction (..), BattlePokemon (..), BattlePhase (..), BattleState (..))
import Game.Logic (getAttackStat, getDefenseStat, getTypeEffectiveness, resolveDamage)
import Game.Move (Move (..))
import Game.Pokemon (Pokemon (..))
import Game.Trainer (AIDifficulty (..))
import Game.Types (Stats (..), Status (..))
import System.Random (StdGen, randomR)

type FeatureVector = [Float]

data QWeights = QWeights
  { qBias :: Float,
    qCoeffs :: [Float]
  }
  deriving (Show, Eq)

-- | ToDo 1 bootstrap:
--   small handcrafted prior so inference is already meaningful before training.
defaultQWeights :: QWeights
defaultQWeights =
  QWeights
    { qBias = 0.0,
      qCoeffs =
        [ 0.15, -- self hp ratio
          -0.10, -- enemy hp ratio (prefer finishing low targets)
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
  let moveCount = length (bpMoves (enemyActive bState))
      moveActions = [ActionMove i | i <- [0 .. moveCount - 1]]
      availableSwitches =
        [ ActionSwitch benchIdx
          | (benchIdx, bp) <- zip [0 ..] (enemyBench bState),
            bpStatus bp /= Fainted,
            shouldConsiderSwitch bState benchIdx bp
        ]
   in case phase bState of
        BattleEnded _ -> []
        WaitingForForcedPlayerSwitch -> moveActions ++ availableSwitches
        _ -> moveActions ++ availableSwitches

extractFeatures :: BattleState -> BattleAction -> FeatureVector
extractFeatures bState action =
  [ selfHpRatio,
    enemyHpRatio,
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
    selfHpRatio = safeRatio (bpHp selfActive) (bpMaxHp selfActive)
    enemyHpRatio = safeRatio (bpHp opponentActive) (bpMaxHp opponentActive)
    speedAdvantage =
      normalizeSigned $
        fromIntegral (speed (pStats (bpOriginal selfActive)) - speed (pStats (bpOriginal opponentActive))) / 100.0
    (expectedDamageRatio, normalizedTypeEffectiveness, koEstimate) = case action of
      ActionMove moveIdx ->
        case safeAt moveIdx (bpMoves selfActive) of
          Nothing -> (0.0, 0.5, 0.0)
          Just selectedMove ->
            let dmg = estimateDamage selfActive opponentActive selectedMove
                maxOppHp = max 1 (bpMaxHp opponentActive)
                dmgRatio = clamp01 (fromIntegral dmg / fromIntegral maxOppHp)
                eff = moveTypeEffectiveness selectedMove opponentActive
                effNorm = normalizeTypeEffectiveness eff
                ko = if dmg >= bpHp opponentActive then 1.0 else 0.0
             in (dmgRatio, effNorm, ko)
      ActionSwitch benchIdx ->
        case safeAt benchIdx (enemyBench bState) of
          Nothing -> (0.0, 0.5, 0.0)
          Just _incoming -> (0.0, 0.5, 0.0)
    (switchDefensiveGain, switchOffensiveGain) = case action of
      ActionSwitch benchIdx ->
        case safeAt benchIdx (enemyBench bState) of
          Nothing -> (0.0, 0.0)
          Just incoming ->
            ( estimateSwitchDefensiveGain selfActive incoming opponentActive,
              estimateSwitchOffensiveGain selfActive incoming opponentActive
            )
      _ -> (0.0, 0.0)
    isMoveAction = case action of
      ActionMove _ -> 1.0
      _ -> 0.0
    isSwitchAction = case action of
      ActionSwitch _ -> 1.0
      _ -> 0.0

qValue :: QWeights -> BattleState -> BattleAction -> Float
qValue weights bState action =
  qBias weights + sum (zipWith (*) (qCoeffs weights) (extractFeatures bState action))

chooseActionGreedy :: QWeights -> BattleState -> Maybe BattleAction
chooseActionGreedy weights bState =
  case candidateActions bState of
    [] -> Nothing
    actions ->
      let scored = [(action, qValue weights bState action) | action <- actions]
       in Just (fst (maximumBy (comparing snd) scored))

chooseActionEpsilon :: StdGen -> Float -> QWeights -> BattleState -> (Maybe BattleAction, StdGen)
chooseActionEpsilon rng epsilon weights bState =
  case candidateActions bState of
    [] -> (Nothing, rng)
    actions ->
      let eps = clamp01 epsilon
          (roll, rngAfterRoll) = randomR (0.0 :: Float, 1.0 :: Float) rng
       in if roll < eps
            then
              let (pickedIdx, rngAfterPick) = randomR (0, length actions - 1) rngAfterRoll
               in (Just (actions !! pickedIdx), rngAfterPick)
            else (chooseActionGreedy weights bState, rngAfterRoll)

difficultyExplorationRate :: AIDifficulty -> Float
difficultyExplorationRate difficulty = case difficulty of
  DifficultyEasy -> 0.35
  DifficultyMedium -> 0.20
  DifficultyHard -> 0.08
  DifficultyExtreme -> 0.00

safeAt :: Int -> [a] -> Maybe a
safeAt idx xs
  | idx < 0 || idx >= length xs = Nothing
  | otherwise = Just (xs !! idx)

safeRatio :: Int -> Int -> Float
safeRatio num den
  | den <= 0 = 0.0
  | otherwise = clamp01 (fromIntegral num / fromIntegral den)

clamp01 :: Float -> Float
clamp01 x
  | x < 0.0 = 0.0
  | x > 1.0 = 1.0
  | otherwise = x

normalizeSigned :: Float -> Float
normalizeSigned x
  | x < -1.0 = -1.0
  | x > 1.0 = 1.0
  | otherwise = x

moveTypeEffectiveness :: Move -> BattlePokemon -> Float
moveTypeEffectiveness mv target =
  let targetPrimaryType = head (pType (bpOriginal target))
   in getTypeEffectiveness (mType mv) targetPrimaryType

normalizeTypeEffectiveness :: Float -> Float
normalizeTypeEffectiveness eff
  | eff <= 0.0 = 0.0
  | eff <= 0.5 = 0.25
  | eff <= 1.0 = 0.5
  | eff <= 2.0 = 1.0
  | otherwise = 1.0

estimateDamage :: BattlePokemon -> BattlePokemon -> Move -> Int
estimateDamage attacker defender selectedMove =
  let level = bpLevel attacker
      atkStat = getAttackStat selectedMove (pStats (bpOriginal attacker))
      defStat = getDefenseStat selectedMove (pStats (bpOriginal defender))
      defType = head (pType (bpOriginal defender))
   in resolveDamage level atkStat defStat selectedMove defType

estimateSwitchDefensiveGain :: BattlePokemon -> BattlePokemon -> BattlePokemon -> Float
estimateSwitchDefensiveGain current incoming opponent =
  let currentIncomingThreat = bestTypeEffectivenessAgainst current opponent
      incomingIncomingThreat = bestTypeEffectivenessAgainst incoming opponent
      gain = currentIncomingThreat - incomingIncomingThreat
   in normalizeSigned (gain / 2.0)

estimateSwitchOffensiveGain :: BattlePokemon -> BattlePokemon -> BattlePokemon -> Float
estimateSwitchOffensiveGain current incoming opponent =
  let currentOffense = bestTypeEffectivenessFor current opponent
      incomingOffense = bestTypeEffectivenessFor incoming opponent
      gain = incomingOffense - currentOffense
   in normalizeSigned (gain / 2.0)

bestTypeEffectivenessFor :: BattlePokemon -> BattlePokemon -> Float
bestTypeEffectivenessFor attacker defender =
  case bpMoves attacker of
    [] -> 1.0
    moves -> maximum [moveTypeEffectiveness mv defender | mv <- moves]

bestTypeEffectivenessAgainst :: BattlePokemon -> BattlePokemon -> Float
bestTypeEffectivenessAgainst defender attacker =
  case bpMoves attacker of
    [] -> 1.0
    moves -> maximum [moveTypeEffectiveness mv defender | mv <- moves]

shouldConsiderSwitch :: BattleState -> Int -> BattlePokemon -> Bool
shouldConsiderSwitch bState _benchIdx incoming =
  let current = enemyActive bState
      opponent = playerActive bState
      hpRatio = safeRatio (bpHp current) (bpMaxHp current)
      defGain = estimateSwitchDefensiveGain current incoming opponent
      offGain = estimateSwitchOffensiveGain current incoming opponent
      meaningfulGain = defGain > 0.20 || offGain > 0.25
      emergency = hpRatio < 0.35
   in meaningfulGain || emergency
