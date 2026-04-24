module Game.AITraining
  ( EpochMetrics (..),
    TrainingRunSummary (..),
    checkpointSelectionScore,
    runTrainingEpochs,
    runTrainingEpochsDetailed,
  )
where

import Game.AIHyperParams (RewardWeights (..), TrainingHyperParams (..))
import Game.AIModel
  ( QWeights (..),
    candidateActions,
    chooseActionEpsilon,
    defaultQWeights,
    extractFeatures,
    qValue,
  )
import Game.Battle
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
    Winner (..),
    executeTurn,
    initBattle,
  )
import Game.Trainer (Trainer (..), allTrainers)
import System.Random (StdGen, randomR)

data EpochMetrics = EpochMetrics
  { emEpochIndex :: Int,
    emEpsilon :: Float,
    emAverageReward :: Float,
    emWinRate :: Float,
    emAverageTurns :: Float
  }
  deriving (Show, Eq)

data TrainingRunSummary = TrainingRunSummary
  { trsFinalWeights :: QWeights,
    trsCanonicalWeights :: QWeights,
    trsCanonicalEpoch :: Int,
    trsCanonicalScore :: Float,
    trsMetrics :: [EpochMetrics]
  }
  deriving (Show, Eq)

runTrainingEpochs :: StdGen -> TrainingHyperParams -> Int -> (QWeights, [EpochMetrics], StdGen)
runTrainingEpochs rng params epochs =
  let (summary, nextRng) = runTrainingEpochsDetailed rng params epochs
   in (trsCanonicalWeights summary, trsMetrics summary, nextRng)

runTrainingEpochsDetailed :: StdGen -> TrainingHyperParams -> Int -> (TrainingRunSummary, StdGen)
runTrainingEpochsDetailed rng params epochs =
  let usableTrainers = trainingTrainers
      initialWeights = defaultQWeights
      initialBest = (initialWeights, -1, -1.0e30 :: Float)
      (finalWeights, bestWeights, bestEpoch, bestScore, metrics, nextRng) =
        trainEpochLoop rng params usableTrainers initialWeights initialBest [] 0 epochs
      summary =
        TrainingRunSummary
          { trsFinalWeights = finalWeights,
            trsCanonicalWeights = bestWeights,
            trsCanonicalEpoch = bestEpoch,
            trsCanonicalScore = bestScore,
            trsMetrics = metrics
          }
   in (summary, nextRng)

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
     in trainEpochLoop
          rngAfterEpoch
          params
          trainers
          weightsAfterEpoch
          nextBestState
          (epochMetrics : metrics)
          (epochIdx + 1)
          totalEpochs

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
          { emEpochIndex = epochIdx,
            emEpsilon = epsilon,
            emAverageReward = totalReward / episodeCountF,
            emWinRate = fromIntegral totalWins / episodeCountF,
            emAverageTurns = fromIntegral totalTurns / episodeCountF
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
   in runEpisodes
        rngAfterEpisode
        params
        trainers
        weightsAfterEpisode
        epsilon
        (remaining - 1)
        (accReward + episodeReward)
        nextWins
        (accTurns + episodeTurns)

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
    rewardWeights = hpRewardWeights params

    episodeLoop rngNow weightsNow bState turnAcc rewardAcc =
      case phase bState of
        BattleEnded winner ->
          let enemyWon = winner == EnemyWon
           in (weightsNow, rewardAcc, enemyWon, turnAcc, rngNow)
        _ | turnAcc >= maxTurns ->
          (weightsNow, rewardAcc, False, turnAcc, rngNow)
        _ ->
          let mirroredState = mirrorBattleState bState
              (playerMaybeAction, rngAfterPlayerPick) = chooseActionEpsilon rngNow epsilon weightsNow mirroredState
              (enemyMaybeAction, rngAfterEnemyPick) = chooseActionEpsilon rngAfterPlayerPick epsilon weightsNow bState
              playerAction = maybe (ActionMove 0) id playerMaybeAction
              enemyAction = maybe (ActionMove 0) id enemyMaybeAction
              (nextState, rngAfterTurn) = executeTurn rngAfterEnemyPick bState playerAction enemyAction

              enemyReward = transitionReward rewardWeights bState nextState
              playerReward = transitionReward rewardWeights mirroredState (mirrorBattleState nextState)

              weightsAfterEnemy =
                tdUpdate params weightsNow bState enemyAction enemyReward nextState
              weightsAfterBoth =
                tdUpdate params weightsAfterEnemy mirroredState playerAction playerReward (mirrorBattleState nextState)
           in episodeLoop rngAfterTurn weightsAfterBoth nextState (turnAcc + 1) (rewardAcc + enemyReward)

tdUpdate :: TrainingHyperParams -> QWeights -> BattleState -> BattleAction -> Float -> BattleState -> QWeights
tdUpdate params weights s action reward sNext =
  let prediction = qValue weights s action
      nextBest = bestQValue weights sNext
      target = reward + hpDiscountGamma params * nextBest
      delta = clampSigned (hpGradientClip params) (target - prediction)
      alpha = hpLearningRateAlpha params
      feats = extractFeatures s action
      bias' = qBias weights + alpha * delta
      coeffs' = zipWith (\w x -> w + alpha * delta * x) (qCoeffs weights) feats
   in weights {qBias = bias', qCoeffs = coeffs'}

bestQValue :: QWeights -> BattleState -> Float
bestQValue weights bState =
  case phase bState of
    BattleEnded _ -> 0.0
    _ ->
      let actions = candidateActions bState
       in case actions of
            [] -> 0.0
            _ -> maximum [qValue weights bState action | action <- actions]

transitionReward :: RewardWeights -> BattleState -> BattleState -> Float
transitionReward rw prevState nextState =
  let prevEnemy = enemyActive prevState
      prevPlayer = playerActive prevState
      nextEnemy = enemyActive nextState
      nextPlayer = playerActive nextState

      damageInflicted =
        safeRatio (bpHp prevPlayer - bpHp nextPlayer) (bpMaxHp prevPlayer)
      damageReceived =
        safeRatio (bpHp prevEnemy - bpHp nextEnemy) (bpMaxHp prevEnemy)

      enemyKO = if bpHp nextPlayer <= 0 then 1.0 else 0.0
      selfKO = if bpHp nextEnemy <= 0 then 1.0 else 0.0
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
mirrorPhase bp = case bp of
  BattleEnded winner -> BattleEnded (mirrorWinner winner)
  other -> other

mirrorWinner :: Winner -> Winner
mirrorWinner winner = case winner of
  PlayerWon -> EnemyWon
  EnemyWon -> PlayerWon

sampleInitialBattle :: StdGen -> [Trainer] -> (BattleState, StdGen)
sampleInitialBattle rng trainers =
  let count = length trainers
      safeTrainers = if count > 0 then trainers else error "No trainers available for AI training."
      (enemyIdx, rng1) = randomR (0, length safeTrainers - 1) rng
      (playerIdx, rng2) = randomR (0, length safeTrainers - 1) rng1
      enemyTrainer = safeTrainers !! enemyIdx
      playerTrainer = safeTrainers !! playerIdx
      playerTeam = tTeamIds playerTrainer
      battleState0 = initBattle playerTeam enemyTrainer
   in (battleState0, rng2)

trainingTrainers :: [Trainer]
trainingTrainers =
  filter (\t -> length (tTeamIds t) >= 1) allTrainers

epsilonAtEpoch :: TrainingHyperParams -> Int -> Float
epsilonAtEpoch params epochIdx =
  let startEps = hpEpsilonStart params
      minEps = hpEpsilonMin params
      decay = hpEpsilonDecay params
      decayed = startEps * (decay ^^ epochIdx)
   in max minEps decayed

checkpointSelectionScore :: EpochMetrics -> Float
checkpointSelectionScore metrics =
  let rewardTerm = emAverageReward metrics
      winTerm = emWinRate metrics * 10.0
      paceTerm = negate (emAverageTurns metrics * 0.05)
   in rewardTerm + winTerm + paceTerm

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
