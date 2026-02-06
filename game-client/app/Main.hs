module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map
import System.Random (mkStdGen, getStdGen, StdGen)

import Engine.Keys (handleInput, Screen(..), GameState(..))

import Engine.Common (loadPngSafe)
import Game.Pokemon (allPokemon, Pokemon(..))
import Game.Trainer (allTrainers, Trainer(..))

import Screens.StartScreen (drawStartScreen)
import Screens.MenuScreen (drawMenuScreen)
import Screens.PokedexScreen (drawPokedexScreen)
import Screens.PokemonScreen (drawPokemonScreen)
import Screens.MultiplayerScreen (drawMultiplayerScreen)
import Screens.TeamSelectScreen (drawTeamSelectScreen)
import Screens.OpponentSelectScreen (drawOpponentSelectScreen)

--------------------------------------------------------------------------------
-- MODELO DE DATOS (ESTADO)
--------------------------------------------------------------------------------
initialState :: Picture -> Picture -> Picture -> Map.Map Int Picture -> Map.Map Int Picture -> StdGen -> GameState
initialState startBg menuBg logo pokemonSprites trainerSprites rng = GameState
    { currentScreen = StartScreen
    , selectedOption = 0
    , selectedPokemon = 1
    , playerTeam = []
    , selectedTrainer = Nothing
    , selectedTrainerIndex = 0
    , startBgImage = startBg
    , menuBgImage = menuBg
    , logoImage = logo
    , pokemonSprites = pokemonSprites
    , trainerSprites = trainerSprites
    , rngSeed = rng
    }

--------------------------------------------------------------------------------
-- VISTA (RENDER)
--------------------------------------------------------------------------------
draw :: GameState -> Picture
draw state = case currentScreen state of
    StartScreen -> drawStartScreen (startBgImage state)
    Menu        -> drawMenuScreen (menuBgImage state) (logoImage state) (selectedOption state)
    Pokedex -> 
        let maybeSprite = Map.lookup (selectedPokemon state) (pokemonSprites state)
        in drawPokedexScreen (menuBgImage state) (logoImage state) (selectedPokemon state) maybeSprite
    PokemonDetail -> 
        let maybeSprite = Map.lookup (selectedPokemon state) (pokemonSprites state)
        in drawPokemonScreen (menuBgImage state) (logoImage state) (selectedPokemon state) maybeSprite
    Multiplayer -> drawMultiplayerScreen
    TeamSelect  -> drawTeamSelectScreen (menuBgImage state) (logoImage state) (selectedPokemon state) (playerTeam state) (pokemonSprites state)
    OpponentSelect -> drawOpponentSelectScreen (menuBgImage state) (logoImage state) (selectedTrainerIndex state) (pokemonSprites state) (trainerSprites state)

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
    menuBg  <- loadBMP "game-client/assets/images/main_screen.bmp"
    logo    <- loadBMP "game-client/assets/images/logo.bmp"
    
    -- 2. Cargar Sprites de Pokemon (Dinámico)
    putStrLn "Cargando Pokedex..."
    pokemonSpriteList <- loadPokemonSprites allPokemon
    let pokemonSpriteMap = Map.fromList pokemonSpriteList
    
    putStrLn $ "Se cargaron " ++ show (length pokemonSpriteList) ++ " pokemons."
    
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
        (initialState startBg menuBg logo pokemonSpriteMap trainerSpriteMap rng)
        draw 
        handleInput 
        update

--------------------------------------------------------------------------------
-- HELPERS: Carga masiva de sprites
--------------------------------------------------------------------------------
loadPokemonSprites :: [Pokemon] -> IO [(Int, Picture)]
loadPokemonSprites [] = return []
loadPokemonSprites (p:ps) = do
    pic <- loadPngSafe (frontSprite p)    
    rest <- loadPokemonSprites ps
    
    return ((pId p, pic) : rest)

loadTrainerSprites :: [Trainer] -> IO [(Int, Picture)]
loadTrainerSprites [] = return []
loadTrainerSprites (t:ts) = do
    pic <- loadPngSafe (tSprite t)
    rest <- loadTrainerSprites ts
    
    return ((tId t, pic) : rest)