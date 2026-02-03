module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map

-- Importar tipos y control de teclas desde el engine
import Engine.Keys (handleInput, Screen(..), GameState(..))

-- Importar recursos y lógica
import Engine.Common (loadPngSafe)
import Game.Pokemon (allPokemon, Pokemon(..))

-- Importar pantallas
import Screens.StartScreen (drawStartScreen)
import Screens.MenuScreen (drawMenuScreen)
import Screens.PokedexScreen (drawPokedexScreen)
import Screens.PokemonScreen (drawPokemonScreen)
import Screens.MultiplayerScreen (drawMultiplayerScreen)
import Screens.AIScreen (drawAIScreen)

-- 1. MODELO DE DATOS (ESTADO)
--------------------------------------------------------------------------------
-- Estado inicial: Pantalla de inicio, opción 0, y las imágenes cargadas
initialState :: Picture -> Picture -> Picture -> Map.Map Int Picture -> GameState
initialState startBg menuBg logo sprites = GameState
    { currentScreen = StartScreen
    , selectedOption = 0
    , selectedPokemon = 1
    , startBgImage = startBg
    , menuBgImage = menuBg
    , logoImage = logo
    , pokemonSprites = sprites
    }

-- 2. VISTA (RENDER)
-- Esta función toma el Estado y devuelve una "Picture" (dibujo)
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
    PlayingAI   -> drawAIScreen

-- 3. LOGICA DE TIEMPO
--------------------------------------------------------------------------------
update :: Float -> GameState -> GameState
update _ state = state

-- 4. MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== POKEMONAD INIT ==="
    
    -- 1. Cargar Fondos (Imágenes estáticas)
    putStrLn "Cargando interfaces..."
    startBg <- loadBMP "game-client/assets/images/background.bmp"     -- Fondo de pantalla de inicio
    menuBg  <- loadBMP "game-client/assets/images/main_screen.bmp"    -- Fondo del menú principal
    logo    <- loadBMP "game-client/assets/images/logo.bmp"
    
    -- 2. Cargar Sprites de Pokemon (Dinámico)
    putStrLn "Cargando Pokedex..."
    spriteList <- loadPokemonSprites allPokemon
    let spriteMap = Map.fromList spriteList
    
    putStrLn $ "Se cargaron " ++ show (length spriteList) ++ " pokemons."
    putStrLn "Iniciando Ventana..."
    
    let window = InWindow "Pokemonad P2P" (1280, 720) (100, 100)
    
    play 
        window 
        black 
        30 
        (initialState startBg menuBg logo spriteMap)
        draw 
        handleInput 
        update

-- HELPER: Carga masiva de sprites
loadPokemonSprites :: [Pokemon] -> IO [(Int, Picture)]
loadPokemonSprites [] = return []
loadPokemonSprites (p:ps) = do
    -- Cargamos la imagen del pokemon actual
    pic <- loadPngSafe (frontSprite p)
    
    -- Cargamos el resto recursivamente
    rest <- loadPokemonSprites ps
    
    -- Devolvemos la tupla (ID, Foto)
    return ((pId p, pic) : rest)