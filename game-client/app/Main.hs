module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Concurrent.STM.TVar (newTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import qualified Data.Map as Map
import Engine.Common (loadPngSafe)
import Engine.GameState (BattleMenuType (..), GameState (..), MultiplayerIntent (..), Screen (..))
import Engine.Keys (handleInput, handleTick)
import Engine.World
  ( NetConnAsync (..),
    NetSubState (..),
    World (..),
    disconnectNetWorld,
    drainNetInbox,
    mergeNetAsync,
  )
import Network.Socket (HostName, PortNumber, SockAddr, Socket)
import P2P.Communication (connectTo, forkRecvLoop, listenAndAccept)
import Game.Pokemon (Pokemon (..), allPokemon)
import Game.Trainer (Trainer (..), allTrainers)
import Graphics.Gloss (Display (InWindow), Picture, black, loadBMP)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game (Event)
import Screens.BattleScreen (drawBattleScreen)
import Screens.MenuScreen (drawMenuScreen)
import Screens.MultiplayerScreen (drawMultiplayerScreen)
import Screens.OpponentSelectScreen (drawOpponentSelectScreen)
import Screens.PokedexScreen (drawPokedexScreen)
import Screens.PokemonScreen (drawPokemonScreen)
import Screens.StartScreen (drawStartScreen)
import Screens.TeamSelectScreen (drawTeamSelectScreen)
import System.Random (StdGen, getStdGen)

--------------------------------------------------------------------------------
-- MODELO DE DATOS (ESTADO)
--------------------------------------------------------------------------------
initialState :: Picture -> Picture -> Picture -> Map.Map Int Picture -> Map.Map Int Picture -> Map.Map Int Picture -> [Picture] -> StdGen -> GameState
initialState startBg menuBg logo pokemonFrontSprites pokemonBackSprites trainerSprites battleBgs rng =
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
      multiplayerHost = "127.0.0.1",
      multiplayerPort = "7878",
      multiplayerRow = 0,
      multiplayerPending = Nothing,
      multiplayerError = Nothing
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
  TeamSelect -> drawTeamSelectScreen (menuBgImage state) (logoImage state) (selectedPokemon state) (playerTeam state) (pokemonFrontSprites state)
  OpponentSelect -> drawOpponentSelectScreen (menuBgImage state) (logoImage state) (selectedTrainerIndex state) (pokemonFrontSprites state) (trainerSprites state)
  BattleScreen -> drawBattleScreen (battleBackgrounds state) (currentBattleBg state) (battleState state) (pokemonFrontSprites state) (pokemonBackSprites state) (battleMenuIndex state) (battleMenuType state) (battleMoveIndex state)

drawWorld :: World -> IO Picture
drawWorld w = pure $ draw (worldGame w) (netSubState w)

handleWorldInput :: Event -> World -> IO World
handleWorldInput ev w = do
  let g0 = worldGame w
      g1 = handleInput ev g0
      leftMP = currentScreen g0 == Multiplayer && currentScreen g1 /= Multiplayer
  w1 <- if leftMP then disconnectNetWorld w else pure w
  let w2 = w1 {worldGame = g1}
  case multiplayerPending g1 of
    Nothing -> pure w2
    Just intent ->
      let g2 = g1 {multiplayerPending = Nothing}
       in startMultiplayerNet intent (w2 {worldGame = g2})

handleWorldTick :: Float -> World -> IO World
handleWorldTick dt w = do
  wMerged <- mergeNetAsync w
  wDrained <- drainNetInbox wMerged
  pure wDrained {worldGame = handleTick dt (worldGame wDrained)}

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

  rng <- getStdGen

  putStrLn "Iniciando Ventana..."

  netInQueue <- newTQueueIO
  netAsync <- newTVarIO Nothing
  let window = InWindow "Pokemonad P2P" (1280, 720) (100, 100)
      game0 =
        initialState startBg menuBg logo pokemonFrontSpriteMap pokemonBackSpriteMap trainerSpriteMap battleBgs rng
      world0 =
        World
          { worldGame = game0,
            netInQueue = netInQueue,
            netSubState = NetDisconnected,
            netSocket = Nothing,
            netConnAsync = netAsync
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