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
import Pokemonad.Battle.Turn (executeTurn)
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
      initialBest = (initialWeights, -1, -1.0e30 :: Float)
      (finalWeights, bestWeights, bestEpoch, bestScore, metrics, nextRng) =
        trainEpochLoop rng params usableTrainers initialWeights initialBest [] 0 epochs
   in ( TrainingRunSummary
          { summaryFinalWeights = finalWeights,
            summaryCanonicalWeights = bestWeights,
            summaryCanonicalEpoch = bestEpoch,
            summaryCanonicalScore = bestScore,
            summaryMetrics = metrics
          },
        nextRng
      )

trainEpochLoop ::
  StdGen ->
  TrainingHyperParams ->
  [Trainer] ->
  QWeights ->
  (QWeights, Int, Float) ->
  [EpochMetrics] ->
  Int ->
  Int ->
  (QWeights, QWeights, Int, Float, [EpochMetrics], StdGen)
trainEpochLoop rng _params _trainers weights bestState metrics epochIdx totalEpochs
  | epochIdx >= totalEpochs =
      let (bestWeights, bestEpoch, bestScore) = bestState
          fallbackEpoch = if bestEpoch < 0 then max 0 (totalEpochs - 1) else bestEpoch
          fallbackScore = if bestEpoch < 0 then 0.0 else bestScore
       in (weights, bestWeights, fallbackEpoch, fallbackScore, reverse metrics, rng)
trainEpochLoop rng params trainers weights bestState metrics epochIdx totalEpochs =
  let epsilon = epsilonAtEpoch params epochIdx
      (weightsAfterEpoch, epochMetrics, rngAfterEpoch) = runSingleEpoch rng params trainers weights epochIdx epsilon
      currentScore = checkpointSelectionScore epochMetrics
      (prevBestWeights, prevBestEpoch, prevBestScore) = bestState
      nextBestState =
        if currentScore > prevBestScore
          then (weightsAfterEpoch, epochIdx, currentScore)
          else (prevBestWeights, prevBestEpoch, prevBestScore)
   in trainEpochLoop rngAfterEpoch params trainers weightsAfterEpoch nextBestState (epochMetrics : metrics) (epochIdx + 1) totalEpochs

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
      (finalWeights, totalReward, totalWins, totalTurns, rngAfter) =
        runEpisodes rng params trainers weights epsilon episodesPerEpoch 0.0 0 0
      episodeCountF = fromIntegral episodesPerEpoch :: Float
      metrics =
        EpochMetrics
          { epochIndex = epochIdx,
            epochEpsilon = epsilon,
            epochAverageReward = totalReward / episodeCountF,
            epochWinRate = fromIntegral totalWins / episodeCountF,
            epochAverageTurns = fromIntegral totalTurns / episodeCountF
          }
   in (finalWeights, metrics, rngAfter)

runEpisodes ::
  StdGen ->
  TrainingHyperParams ->
  [Trainer] ->
  QWeights ->
  Float ->
  Int ->
  Float ->
  Int ->
  Int ->
  (QWeights, Float, Int, Int, StdGen)
runEpisodes rng _params _trainers weights _epsilon 0 accReward accWins accTurns =
  (weights, accReward, accWins, accTurns, rng)
runEpisodes rng params trainers weights epsilon remaining accReward accWins accTurns =
  let (battleState0, rngAfterInit) = sampleInitialBattle rng trainers
      (weightsAfterEpisode, episodeReward, didWinEnemySide, episodeTurns, rngAfterEpisode) =
        runSelfPlayEpisode rngAfterInit params weights epsilon battleState0
      nextWins = if didWinEnemySide then accWins + 1 else accWins
   in runEpisodes rngAfterEpisode params trainers weightsAfterEpisode epsilon (remaining - 1) (accReward + episodeReward) nextWins (accTurns + episodeTurns)

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
              (nextState, rng3) = executeTurn rng2 bState playerAction enemyAction
              enemyReward = transitionReward rw bState nextState
              weightsAfterEnemy = tdUpdate params weightsNow bState enemyAction enemyReward nextState
              weightsAfterBoth = tdUpdate params weightsAfterEnemy mirroredState playerAction (transitionReward rw mirroredState (mirrorBattleState nextState)) (mirrorBattleState nextState)
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
