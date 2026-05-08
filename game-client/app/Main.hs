module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Data.Map as Map
import Engine.Common (loadPngSafe)
import Engine.GameState (AITrainingResult (..), BattleMenuType (..), GameState (..), MultiplayerIntent (..), Screen (..))
import Engine.Keys (handleInput, handleTick)
import Engine.World
  ( NetConnAsync (..),
    NetSubState (..),
    World (..),
    disconnectNetWorld,
    drainNetInbox,
    mergeNetAsync,
  )
import Game.AI
  ( AICheckpointData (..),
    EpochMetrics (..),
    TrainingRunSummary (..),
    defaultCheckpointPath,
    defaultQWeights,
    defaultTrainingHyperParams,
    loadCheckpointData,
    runTrainingEpochsDetailed,
    saveCheckpointData,
  )
import Game.Battle (BattlePhase (..), BattleState (..), Winner (..))
import Game.Pokemon (Pokemon (..), allPokemon)
import Game.Trainer (Trainer (..), allTrainers)
import Graphics.Gloss (Display (InWindow), Picture, black, loadBMP)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..), KeyState (..), SpecialKey (..))
import Network.Socket (HostName, PortNumber, SockAddr, Socket)
import P2P.Communication (connectTo, forkRecvLoop, listenAndAccept)
import Screens.AISimulatorScreen (drawAISimulatorScreen)
import Screens.BattleEndScreen (drawBattleEndScreen)
import Screens.BattleScreen (drawBattleScreen)
import Screens.MenuScreen (drawMenuScreen)
import Screens.MultiplayerScreen (drawMultiplayerScreen)
import Screens.OpponentSelectScreen (drawOpponentSelectScreen)
import Screens.PokedexScreen (drawPokedexScreen)
import Screens.PokemonScreen (drawPokemonScreen)
import Screens.StartScreen (drawStartScreen)
import Screens.TeamSelectScreen (drawTeamSelectScreen)
import System.Random (StdGen, initStdGen)

--------------------------------------------------------------------------------
-- MODELO DE DATOS (ESTADO)
--------------------------------------------------------------------------------
initialState :: Picture -> Picture -> Picture -> Picture -> Picture -> Map.Map Int Picture -> Map.Map Int Picture -> Map.Map Int Picture -> [Picture] -> StdGen -> GameState
initialState startBg menuBg logo winnerBg loserBg pokemonFrontSprites pokemonBackSprites trainerSprites battleBgs rng =
  GameState
    { currentScreen = StartScreen,
      selectedOption = 0,
      selectedPokemon = 1,
      playerTeam = [],
      selectedTrainer = Nothing,
      selectedTrainerIndex = 0,
      startBgImage = startBg,
      menuBgImage = menuBg,
      logoImage = logo,
      winnerBgImage = winnerBg,
      loserBgImage = loserBg,
      pokemonFrontSprites = pokemonFrontSprites,
      pokemonBackSprites = pokemonBackSprites,
      trainerSprites = trainerSprites,
      battleState = Nothing,
      battleBackgrounds = battleBgs,
      currentBattleBg = 0,
      battleMenuIndex = 0,
      rngSeed = rng,
      holdingUp = False,
      holdingDown = False,
      scrollTimer = 0.0,
      battleMenuType = MainBattleMenu,
      battleMoveIndex = 0,
      battleBenchIndex = 0,
      multiplayerHost = "127.0.0.1",
      multiplayerPort = "7878",
      multiplayerRow = 0,
      multiplayerPending = Nothing,
      multiplayerError = Nothing,
      enemyAIWeights = Nothing,
      simulatorTraining = False,
      simulatorStatus = "Ready. Press ENTER to run 100 epochs.",
      simulatorTotalEpochs = 0,
      simulatorLogs = []
    }

--------------------------------------------------------------------------------
-- VISTA (RENDER)
--------------------------------------------------------------------------------
draw :: GameState -> NetSubState -> Picture
draw state netSt = case currentScreen state of
  StartScreen -> drawStartScreen (startBgImage state)
  Menu -> drawMenuScreen (menuBgImage state) (logoImage state) (selectedOption state)
  Pokedex ->
    let maybeSprite = Map.lookup (selectedPokemon state) (pokemonFrontSprites state)
     in drawPokedexScreen (menuBgImage state) (logoImage state) (selectedPokemon state) maybeSprite
  PokemonDetail ->
    let maybeSprite = Map.lookup (selectedPokemon state) (pokemonFrontSprites state)
     in drawPokemonScreen (menuBgImage state) (logoImage state) (selectedPokemon state) maybeSprite
  Multiplayer -> drawMultiplayerScreen (menuBgImage state) (logoImage state) state netSt
  AISimulator -> drawAISimulatorScreen (menuBgImage state) (logoImage state) state
  TeamSelect -> drawTeamSelectScreen (menuBgImage state) (logoImage state) (selectedPokemon state) (playerTeam state) (pokemonFrontSprites state)
  OpponentSelect -> drawOpponentSelectScreen (menuBgImage state) (logoImage state) (selectedTrainerIndex state) (pokemonFrontSprites state) (trainerSprites state)
  BattleScreen -> drawBattleScreen (battleBackgrounds state) (currentBattleBg state) (battleState state) (pokemonFrontSprites state) (pokemonBackSprites state) (battleMenuIndex state) (battleMenuType state) (battleMoveIndex state) (battleBenchIndex state)
  BattleResultScreen ->
    case battleState state of
      Just bState ->
        case phase bState of
          BattleEnded winner ->
            let resultBg = case winner of
                  PlayerWon -> winnerBgImage state
                  _ -> loserBgImage state
             in drawBattleEndScreen resultBg winner (battleState state) (selectedTrainer state) (trainerSprites state) (pokemonFrontSprites state) (pokemonBackSprites state)
          _ -> drawBattleEndScreen (winnerBgImage state) PlayerWon (battleState state) (selectedTrainer state) (trainerSprites state) (pokemonFrontSprites state) (pokemonBackSprites state)
      Nothing -> drawBattleEndScreen (winnerBgImage state) PlayerWon (battleState state) (selectedTrainer state) (trainerSprites state) (pokemonFrontSprites state) (pokemonBackSprites state)

drawWorld :: World -> IO Picture
drawWorld w = pure $ draw (worldGame w) (netSubState w)

handleWorldInput :: Event -> World -> IO World
handleWorldInput ev w = do
  let g0 = worldGame w
  w1 <- launchAITrainingIfRequested ev w
  let g1 = handleInput ev (worldGame w1)
      leftMP = currentScreen g0 == Multiplayer && currentScreen g1 /= Multiplayer
  w2 <- if leftMP then disconnectNetWorld w1 else pure w1
  let w3 = w2 {worldGame = g1}
  case multiplayerPending g1 of
    Nothing -> pure w3
    Just intent ->
      let g2 = g1 {multiplayerPending = Nothing}
       in startMultiplayerNet intent (w3 {worldGame = g2})

launchAITrainingIfRequested :: Event -> World -> IO World
launchAITrainingIfRequested ev w
  | currentScreen gs /= AISimulator = pure w
  | not isEnterDown = pure w
  | simulatorTraining gs = pure w
  | otherwise = do
      let rng = rngSeed gs
      void $ forkIO $ do
        maybeCheckpoint <- loadCheckpointData defaultCheckpointPath
        let priorEpochs = maybe 0 acdTotalEpochs maybeCheckpoint
            priorBestScore = maybe (-1.0e30) acdBestScore maybeCheckpoint
            priorWeights = maybe defaultQWeights acdWeights maybeCheckpoint
            epochsToRun = 100
            (summary, nextRng) =
              runTrainingEpochsDetailed rng defaultTrainingHyperParams epochsToRun
            newTotal = priorEpochs + epochsToRun
            newScore = trsCanonicalScore summary
            (finalWeights, finalScore) =
              if newScore > priorBestScore
                then (trsCanonicalWeights summary, newScore)
                else (priorWeights, priorBestScore)
            newLogs = reverse (map (formatEpochLine priorEpochs) (trsMetrics summary))
            statusMsg =
              if newScore > priorBestScore
                then "New best! Score: " ++ shortFloat newScore ++ " | Total epochs: " ++ show newTotal
                else "No improvement. Best score: " ++ shortFloat finalScore ++ " | Total epochs: " ++ show newTotal
            nextCheckpoint =
              AICheckpointData
                { acdWeights = finalWeights,
                  acdTotalEpochs = newTotal,
                  acdBestScore = finalScore
                }
        saveCheckpointData defaultCheckpointPath nextCheckpoint
        atomically $
          writeTVar (aiTrainingAsync w) $
            Just
              AITrainingResult
                { atrWeights = finalWeights,
                  atrTotalEpochs = newTotal,
                  atrLogs = newLogs,
                  atrStatus = statusMsg,
                  atrRng = nextRng
                }
      pure
        w
          { worldGame =
              gs
                { simulatorTraining = True,
                  simulatorStatus = "Training in progress..."
                }
          }
  where
    gs = worldGame w
    isEnterDown = case ev of
      EventKey (SpecialKey KeyEnter) Down _ _ -> True
      _ -> False

mergeAITraining :: World -> IO World
mergeAITraining w = do
  m <- atomically $ do
    x <- readTVar (aiTrainingAsync w)
    writeTVar (aiTrainingAsync w) Nothing
    pure x
  case m of
    Nothing -> pure w
    Just result -> pure w {worldGame = applyAIResult result (worldGame w)}

applyAIResult :: AITrainingResult -> GameState -> GameState
applyAIResult result gs =
  gs
    { rngSeed = atrRng result,
      enemyAIWeights = Just (atrWeights result),
      simulatorTotalEpochs = atrTotalEpochs result,
      simulatorStatus = atrStatus result,
      simulatorLogs = atrLogs result ++ simulatorLogs gs,
      simulatorTraining = False
    }

formatEpochLine :: Int -> EpochMetrics -> String
formatEpochLine epochOffset m =
  "E"
    ++ show (epochOffset + emEpochIndex m)
    ++ " eps="
    ++ shortFloat (emEpsilon m)
    ++ " reward="
    ++ shortFloat (emAverageReward m)
    ++ " win="
    ++ shortFloat (emWinRate m)
    ++ " turns="
    ++ shortFloat (emAverageTurns m)

shortFloat :: Float -> String
shortFloat x =
  let scaled = fromIntegral (round (x * 100.0) :: Int) / 100.0 :: Float
   in show scaled

handleWorldTick :: Float -> World -> IO World
handleWorldTick dt w = do
  wMerged <- mergeNetAsync w
  wDrained <- drainNetInbox wMerged
  wAI <- mergeAITraining wDrained
  pure wAI {worldGame = handleTick dt (worldGame wAI)}

runListen :: PortNumber -> World -> IO ()
runListen port w = do
  r <- try (listenAndAccept port) :: IO (Either SomeException (Socket, SockAddr))
  case r of
    Left e ->
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnErr (show e))
    Right (sock, _) -> do
      _ <- forkRecvLoop sock (netInQueue w)
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnOk sock NetInLobby)

runConnect :: HostName -> PortNumber -> World -> IO ()
runConnect host port w = do
  r <- try (connectTo host port) :: IO (Either SomeException Socket)
  case r of
    Left e ->
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnErr (show e))
    Right sock -> do
      _ <- forkRecvLoop sock (netInQueue w)
      atomically $ writeTVar (netConnAsync w) (Just $ NetConnOk sock NetInLobby)

startMultiplayerNet :: MultiplayerIntent -> World -> IO World
startMultiplayerNet intent w = case netSubState w of
  NetListening _ ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "Ya estas en escucha."}}
  NetConnecting _ _ ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "Conexion en curso."}}
  NetInLobby ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "Ya conectado."}}
  NetInBattle ->
    pure w {worldGame = (worldGame w) {multiplayerError = Just "En batalla."}}
  NetDisconnected ->
    case intent of
      MPListen port -> do
        void $ forkIO (runListen port w)
        pure
          w
            { netSubState = NetListening (fromIntegral port),
              worldGame = (worldGame w) {multiplayerError = Nothing}
            }
      MPConnect host port -> do
        void $ forkIO (runConnect host port w)
        pure
          w
            { netSubState = NetConnecting host (fromIntegral port),
              worldGame = (worldGame w) {multiplayerError = Nothing}
            }

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== POKEMONAD INIT ==="

  -- 1. Cargar Fondos (Imágenes estáticas)
  putStrLn "Cargando interfaces..."
  startBg <- loadBMP "game-client/assets/images/background.bmp"
  menuBg <- loadBMP "game-client/assets/images/main_screen.bmp"
  logo <- loadBMP "game-client/assets/images/logo.bmp"

  putStrLn "Cargando arenas de batalla..."
  bg1 <- loadBMP "game-client/assets/images/battle_bg_1.bmp"
  bg2 <- loadBMP "game-client/assets/images/battle_bg_2.bmp"
  bg3 <- loadBMP "game-client/assets/images/battle_bg_3.bmp"
  bg4 <- loadBMP "game-client/assets/images/battle_bg_4.bmp"
  bg5 <- loadBMP "game-client/assets/images/battle_bg_5.bmp"
  winnerBg <- loadBMP "game-client/assets/images/winner.bmp"
  loserBg <- loadBMP "game-client/assets/images/loser.bmp"

  let battleBgs = [bg1, bg2, bg3, bg4, bg5]

  -- 2. Cargar Sprites de Pokemon (Dinámico)
  putStrLn "Cargando Pokedex..."
  pokemonFrontSpriteList <- loadPokemonFrontSprites allPokemon
  let pokemonFrontSpriteMap = Map.fromList pokemonFrontSpriteList

  putStrLn $ "Se cargaron " ++ show (length pokemonFrontSpriteList) ++ " pokemons (front sprites)."

  -- 2b. Cargar Back Sprites de Pokemon
  putStrLn "Cargando sprites traseros de Pokemon..."
  pokemonBackSpriteList <- loadPokemonBackSprites allPokemon
  let pokemonBackSpriteMap = Map.fromList pokemonBackSpriteList

  putStrLn $ "Se cargaron " ++ show (length pokemonBackSpriteList) ++ " pokemons (back sprites)."

  -- 3. Cargar Sprites de Trainers
  putStrLn "Cargando Entrenadores..."
  trainerSpriteList <- loadTrainerSprites allTrainers
  let trainerSpriteMap = Map.fromList trainerSpriteList

  putStrLn $ "Se cargaron " ++ show (length trainerSpriteList) ++ " entrenadores."

  rng <- initStdGen

  putStrLn "Iniciando Ventana..."

  netInQueue <- newTQueueIO
  netAsync <- newTVarIO Nothing
  aiAsync <- newTVarIO Nothing
  let window = InWindow "Pokemonad P2P" (1280, 720) (100, 100)
      game0 =
        initialState startBg menuBg logo winnerBg loserBg pokemonFrontSpriteMap pokemonBackSpriteMap trainerSpriteMap battleBgs rng
      world0 =
        World
          { worldGame = game0,
            netInQueue = netInQueue,
            netSubState = NetDisconnected,
            netSocket = Nothing,
            netConnAsync = netAsync,
            aiTrainingAsync = aiAsync
          }

  playIO
    window
    black
    30
    world0
    drawWorld
    handleWorldInput
    handleWorldTick

--------------------------------------------------------------------------------
-- HELPERS: Carga masiva de sprites
--------------------------------------------------------------------------------
loadPokemonFrontSprites :: [Pokemon] -> IO [(Int, Picture)]
loadPokemonFrontSprites [] = return []
loadPokemonFrontSprites (p : ps) = do
  pic <- loadPngSafe (frontSprite p)
  rest <- loadPokemonFrontSprites ps

  return ((pId p, pic) : rest)

loadPokemonBackSprites :: [Pokemon] -> IO [(Int, Picture)]
loadPokemonBackSprites [] = return []
loadPokemonBackSprites (p : ps) = do
  pic <- loadPngSafe (backSprite p)
  rest <- loadPokemonBackSprites ps

  return ((pId p, pic) : rest)

loadTrainerSprites :: [Trainer] -> IO [(Int, Picture)]
loadTrainerSprites [] = return []
loadTrainerSprites (t : ts) = do
  pic <- loadPngSafe (tSprite t)
  rest <- loadTrainerSprites ts

  return ((tId t, pic) : rest)
