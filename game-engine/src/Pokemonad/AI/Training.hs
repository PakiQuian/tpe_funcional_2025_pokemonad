module Pokemonad.AI.Training
  ( EpochMetrics (..),
    TrainingRunSummary (..),
    checkpointSelectionScore,
    runTrainingEpochs,
    runTrainingEpochsFrom,
    runTrainingEpochsDetailed,
    runTrainingEpochsDetailedFrom,
  )
where

import Pokemonad.AI.HyperParams (RewardWeights (..), TrainingHyperParams (..))
import Pokemonad.AI.Model
  ( QWeights (..),
    candidateActions,
    chooseActionEpsilon,
    defaultQWeights,
    extractFeatures,
    qValue,
  )
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
    Winner (..),
    initBattle,
  )
import Pokemonad.Battle.Turn (executeTurn, finalStateOf)
import Pokemonad.Core.Trainer (Trainer (..), allTrainers)
import Pokemonad.Core.Types (HP (..))
import System.Random (StdGen, randomR)

data EpochMetrics = EpochMetrics
  { epochIndex :: Int,
    epochEpsilon :: Float,
    epochAverageReward :: Float,
    epochWinRate :: Float,
    epochAverageTurns :: Float
  }
  deriving (Show, Eq)

data TrainingRunSummary = TrainingRunSummary
  { summaryFinalWeights :: QWeights,
    summaryCanonicalWeights :: QWeights,
    summaryCanonicalEpoch :: Int,
    summaryCanonicalScore :: Float,
    summaryMetrics :: [EpochMetrics]
  }
  deriving (Show, Eq)

-- | Running best-scoring epoch seen during a training run.
data BestCheckpoint = BestCheckpoint
  { bestWeights :: QWeights,
    bestEpoch :: Int,
    bestScore :: Float
  }
  deriving (Show, Eq)

-- | Running totals accumulated across all episodes within one epoch.
data EpisodeTotals = EpisodeTotals
  { totalReward :: Float,
    totalWins :: Int,
    totalTurns :: Int
  }
  deriving (Show, Eq)

emptyEpisodeTotals :: EpisodeTotals
emptyEpisodeTotals = EpisodeTotals {totalReward = 0.0, totalWins = 0, totalTurns = 0}

runTrainingEpochs :: StdGen -> TrainingHyperParams -> Int -> (QWeights, [EpochMetrics], StdGen)
runTrainingEpochs rng params epochs =
  let (summary, nextRng) = runTrainingEpochsDetailed rng params epochs
   in (summaryCanonicalWeights summary, summaryMetrics summary, nextRng)

runTrainingEpochsFrom :: StdGen -> TrainingHyperParams -> Int -> QWeights -> (QWeights, [EpochMetrics], StdGen)
runTrainingEpochsFrom rng params epochs initialWeights =
  let (summary, nextRng) = runTrainingEpochsDetailedFrom rng params epochs initialWeights
   in (summaryCanonicalWeights summary, summaryMetrics summary, nextRng)

runTrainingEpochsDetailed :: StdGen -> TrainingHyperParams -> Int -> (TrainingRunSummary, StdGen)
runTrainingEpochsDetailed rng params epochs =
  runTrainingEpochsDetailedFrom rng params epochs defaultQWeights

runTrainingEpochsDetailedFrom :: StdGen -> TrainingHyperParams -> Int -> QWeights -> (TrainingRunSummary, StdGen)
runTrainingEpochsDetailedFrom rng params epochs initialWeights =
  let usableTrainers = trainingTrainers
      initialBest = BestCheckpoint {bestWeights = initialWeights, bestEpoch = -1, bestScore = -1.0e30}
      (finalWeights, finalBest, metrics, nextRng) =
        trainEpochLoop rng params usableTrainers initialWeights initialBest [] 0 epochs
      neverImproved = bestEpoch finalBest < 0
      fallbackEpoch = if neverImproved then max 0 (epochs - 1) else bestEpoch finalBest
      fallbackScore = if neverImproved then 0.0 else bestScore finalBest
   in ( TrainingRunSummary
          { summaryFinalWeights = finalWeights,
            summaryCanonicalWeights = bestWeights finalBest,
            summaryCanonicalEpoch = fallbackEpoch,
            summaryCanonicalScore = fallbackScore,
            summaryMetrics = metrics
          },
        nextRng
      )

trainEpochLoop ::
  StdGen ->
  TrainingHyperParams ->
  [Trainer] ->
  QWeights ->
  BestCheckpoint ->
  [EpochMetrics] ->
  Int ->
  Int ->
  (QWeights, BestCheckpoint, [EpochMetrics], StdGen)
trainEpochLoop rng _params _trainers weights best metrics epochIdx totalEpochs
  | epochIdx >= totalEpochs = (weights, best, reverse metrics, rng)
trainEpochLoop rng params trainers weights best metrics epochIdx totalEpochs =
  let epsilon = epsilonAtEpoch params epochIdx
      (weightsAfterEpoch, epochMetrics, rngAfterEpoch) = runSingleEpoch rng params trainers weights epochIdx epsilon
      currentScore = checkpointSelectionScore epochMetrics
      improved = currentScore > bestScore best
      nextBest
        | improved = BestCheckpoint {bestWeights = weightsAfterEpoch, bestEpoch = epochIdx, bestScore = currentScore}
        | otherwise = best
   in trainEpochLoop rngAfterEpoch params trainers weightsAfterEpoch nextBest (epochMetrics : metrics) (epochIdx + 1) totalEpochs

runSingleEpoch ::
  StdGen ->
  TrainingHyperParams ->
  [Trainer] ->
  QWeights ->
  Int ->
  Float ->
  (QWeights, EpochMetrics, StdGen)
runSingleEpoch rng params trainers weights epochIdx epsilon =
  let episodesPerEpoch = 20
      (finalWeights, totals, rngAfter) = runEpisodes rng params trainers weights epsilon episodesPerEpoch emptyEpisodeTotals
      episodeCountF = fromIntegral episodesPerEpoch :: Float
      metrics =
        EpochMetrics
          { epochIndex = epochIdx,
            epochEpsilon = epsilon,
            epochAverageReward = totalReward totals / episodeCountF,
            epochWinRate = fromIntegral (totalWins totals) / episodeCountF,
            epochAverageTurns = fromIntegral (totalTurns totals) / episodeCountF
          }
   in (finalWeights, metrics, rngAfter)

runEpisodes ::
  StdGen ->
  TrainingHyperParams ->
  [Trainer] ->
  QWeights ->
  Float ->
  Int ->
  EpisodeTotals ->
  (QWeights, EpisodeTotals, StdGen)
runEpisodes rng _params _trainers weights _epsilon 0 totals = (weights, totals, rng)
runEpisodes rng params trainers weights epsilon remaining totals =
  let (battleState0, rngAfterInit) = sampleInitialBattle rng trainers
      (weightsAfterEpisode, episodeReward, didWinEnemySide, episodeTurns, rngAfterEpisode) =
        runSelfPlayEpisode rngAfterInit params weights epsilon battleState0
      nextTotals =
        totals
          { totalReward = totalReward totals + episodeReward,
            totalWins = totalWins totals + (if didWinEnemySide then 1 else 0),
            totalTurns = totalTurns totals + episodeTurns
          }
   in runEpisodes rngAfterEpisode params trainers weightsAfterEpisode epsilon (remaining - 1) nextTotals

runSelfPlayEpisode ::
  StdGen ->
  TrainingHyperParams ->
  QWeights ->
  Float ->
  BattleState ->
  (QWeights, Float, Bool, Int, StdGen)
runSelfPlayEpisode rng params initialWeights epsilon battleState0 =
  episodeLoop rng initialWeights battleState0 0 0.0
  where
    maxTurns = 200
    rw = rewardWeights params

    episodeLoop rngNow weightsNow bState turnAcc rewardAcc =
      case phase bState of
        BattleEnded winner ->
          (weightsNow, rewardAcc, winner == EnemyWon, turnAcc, rngNow)
        _
          | turnAcc >= maxTurns ->
              (weightsNow, rewardAcc, False, turnAcc, rngNow)
        _ ->
          let mirroredState = mirrorBattleState bState
              (playerMaybeAction, rng1) = chooseActionEpsilon rngNow epsilon weightsNow mirroredState
              (enemyMaybeAction, rng2) = chooseActionEpsilon rng1 epsilon weightsNow bState
              playerAction = maybe (ActionMove 0) id playerMaybeAction
              enemyAction = maybe (ActionMove 0) id enemyMaybeAction
              (nextState, rng3) = finalStateOf bState (executeTurn rng2 bState playerAction enemyAction)
              mirroredNextState = mirrorBattleState nextState
              enemyReward = transitionReward rw bState nextState
              playerReward = transitionReward rw mirroredState mirroredNextState
              weightsAfterEnemy = tdUpdate params weightsNow bState enemyAction enemyReward nextState
              weightsAfterBoth = tdUpdate params weightsAfterEnemy mirroredState playerAction playerReward mirroredNextState
           in episodeLoop rng3 weightsAfterBoth nextState (turnAcc + 1) (rewardAcc + enemyReward)

tdUpdate :: TrainingHyperParams -> QWeights -> BattleState -> BattleAction -> Float -> BattleState -> QWeights
tdUpdate params weights s action reward sNext =
  let adjustedReward = reward - (case action of ActionSwitch _ -> switchPenalty params; _ -> 0.0)
      prediction = qValue weights s action
      nextBest = bestQValue weights sNext
      delta = clampSigned (gradientClip params) (adjustedReward + discountFactor params * nextBest - prediction)
      alpha = learningRate params
      feats = extractFeatures s action
      newBias = weightsBias weights + alpha * delta
      newCoeffs = zipWith (\w x -> w + alpha * delta * x) (weightsCoefficients weights) feats
   in weights {weightsBias = newBias, weightsCoefficients = newCoeffs}

bestQValue :: QWeights -> BattleState -> Float
bestQValue weights bState =
  case phase bState of
    BattleEnded _ -> 0.0
    _ ->
      case candidateActions bState of
        [] -> 0.0
        actions -> maximum [qValue weights bState a | a <- actions]

transitionReward :: RewardWeights -> BattleState -> BattleState -> Float
transitionReward rw prevState nextState =
  let prevEnemy = enemyActive prevState
      prevPlayer = playerActive prevState
      nextEnemy = enemyActive nextState
      nextPlayer = playerActive nextState
      damageInflicted = safeRatio (unHP (battlePokemonHp prevPlayer) - unHP (battlePokemonHp nextPlayer)) (unHP (battlePokemonMaxHp prevPlayer))
      damageReceived = safeRatio (unHP (battlePokemonHp prevEnemy) - unHP (battlePokemonHp nextEnemy)) (unHP (battlePokemonMaxHp prevEnemy))
      enemyKO = if unHP (battlePokemonHp nextPlayer) <= 0 then 1.0 else 0.0
      selfKO = if unHP (battlePokemonHp nextEnemy) <= 0 then 1.0 else 0.0
      terminalReward = case phase nextState of
        BattleEnded PlayerWon -> rewardWinTerminal rw
        BattleEnded EnemyWon -> rewardLoseTerminal rw
        _ -> 0.0
   in rewardDamageInflicted rw * damageInflicted
        + rewardDamageReceived rw * damageReceived
        + rewardEnemyKnockout rw * enemyKO
        + rewardSelfKnockout rw * selfKO
        + terminalReward
        + rewardPerTurnPenalty rw

mirrorBattleState :: BattleState -> BattleState
mirrorBattleState bState =
  bState
    { playerActive = enemyActive bState,
      playerBench = enemyBench bState,
      enemyActive = playerActive bState,
      enemyBench = playerBench bState,
      phase = mirrorPhase (phase bState)
    }

mirrorPhase :: BattlePhase -> BattlePhase
mirrorPhase (BattleEnded winner) = BattleEnded (mirrorWinner winner)
mirrorPhase other = other

mirrorWinner :: Winner -> Winner
mirrorWinner PlayerWon = EnemyWon
mirrorWinner EnemyWon = PlayerWon

sampleInitialBattle :: StdGen -> [Trainer] -> (BattleState, StdGen)
sampleInitialBattle rng trainers =
  let (enemyIdx, rng1) = randomR (0, length trainers - 1) rng
      (playerIdx, rng2) = randomR (0, length trainers - 1) rng1
      enemyTrainer = trainers !! enemyIdx
      playerTrainer = trainers !! playerIdx
   in (initBattle (trainerTeam playerTrainer) enemyTrainer, rng2)

trainingTrainers :: [Trainer]
trainingTrainers = filter (\t -> not (null (trainerTeam t))) allTrainers

epsilonAtEpoch :: TrainingHyperParams -> Int -> Float
epsilonAtEpoch params epochIdx =
  max (epsilonMin params) (epsilonStart params * (epsilonDecay params ^^ epochIdx))

checkpointSelectionScore :: EpochMetrics -> Float
checkpointSelectionScore metrics =
  epochAverageReward metrics + epochWinRate metrics * 10.0 - epochAverageTurns metrics * 0.05

safeRatio :: Int -> Int -> Float
safeRatio num den
  | den <= 0 = 0.0
  | otherwise = fromIntegral num / fromIntegral den

clampSigned :: Float -> Float -> Float
clampSigned limit x
  | limit <= 0 = x
  | x > limit = limit
  | x < (-limit) = -limit
  | otherwise = x
