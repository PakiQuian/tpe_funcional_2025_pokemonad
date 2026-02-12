module Main where

import qualified Data.Map as Map
import Engine.Common (loadPngSafe)
import Engine.Keys (GameState (..), Screen (..), handleInput)
import Game.Pokemon (Pokemon (..), allPokemon)
import Game.Trainer (Trainer (..), allTrainers)
import Graphics.Gloss
  ( Display (InWindow),
    Picture,
    black,
    loadBMP,
    play,
  )
import Graphics.Gloss.Interface.Pure.Game
  ( Display (InWindow),
    Picture,
    black,
    play,
  )
import Screens.BattleScreen (drawBattleScreen)
import Screens.MenuScreen (drawMenuScreen)
import Screens.MultiplayerScreen (drawMultiplayerScreen)
import Screens.OpponentSelectScreen (drawOpponentSelectScreen)
import Screens.PokedexScreen (drawPokedexScreen)
import Screens.PokemonScreen (drawPokemonScreen)
import Screens.StartScreen (drawStartScreen)
import Screens.TeamSelectScreen (drawTeamSelectScreen)
import System.Random (StdGen, getStdGen, mkStdGen)

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
      rngSeed = rng
    }

--------------------------------------------------------------------------------
-- VISTA (RENDER)
--------------------------------------------------------------------------------
draw :: GameState -> Picture
draw state = case currentScreen state of
  StartScreen -> drawStartScreen (startBgImage state)
  Menu -> drawMenuScreen (menuBgImage state) (logoImage state) (selectedOption state)
  Pokedex ->
    let maybeSprite = Map.lookup (selectedPokemon state) (pokemonFrontSprites state)
     in drawPokedexScreen (menuBgImage state) (logoImage state) (selectedPokemon state) maybeSprite
  PokemonDetail ->
    let maybeSprite = Map.lookup (selectedPokemon state) (pokemonFrontSprites state)
     in drawPokemonScreen (menuBgImage state) (logoImage state) (selectedPokemon state) maybeSprite
  Multiplayer -> drawMultiplayerScreen
  TeamSelect -> drawTeamSelectScreen (menuBgImage state) (logoImage state) (selectedPokemon state) (playerTeam state) (pokemonFrontSprites state)
  OpponentSelect -> drawOpponentSelectScreen (menuBgImage state) (logoImage state) (selectedTrainerIndex state) (pokemonFrontSprites state) (trainerSprites state)
  BattleScreen -> drawBattleScreen (battleBackgrounds state) (currentBattleBg state) (battleState state) (pokemonFrontSprites state) (pokemonBackSprites state)

--------------------------------------------------------------------------------
-- LOGICA DE TIEMPO
--------------------------------------------------------------------------------
update :: Float -> GameState -> GameState
update _ state = state

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

  let window = InWindow "Pokemonad P2P" (1280, 720) (100, 100)

  play
    window
    black
    30
    (initialState startBg menuBg logo pokemonFrontSpriteMap pokemonBackSpriteMap trainerSpriteMap battleBgs rng)
    draw
    handleInput
    update

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