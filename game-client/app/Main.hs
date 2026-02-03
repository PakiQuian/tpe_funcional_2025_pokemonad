module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Importar tipos y control de teclas desde el engine
import Engine.Keys (handleInput, Screen(..), GameState(..))

-- Importar módulos de pantallas
import Screens.StartScreen (drawStartScreen)
import Screens.MenuScreen (drawMenuScreen)
import Screens.PokedexScreen (drawPokedexScreen)
import Screens.PokemonScreen (drawPokemonScreen)
import Screens.MultiplayerScreen (drawMultiplayerScreen)
import Screens.AIScreen (drawAIScreen)
import Engine.Common (loadPngSafe)

-- 1. MODELO DE DATOS (ESTADO)
--------------------------------------------------------------------------------
-- Estado inicial: Pantalla de inicio, opción 0, y las imágenes cargadas
initialState :: Picture -> Picture -> Picture -> GameState
initialState startBg menuBg logo = GameState
    { currentScreen = StartScreen
    , selectedOption = 0
    , selectedPokemon = 1
    , startBgImage = startBg
    , menuBgImage = menuBg
    , logoImage = logo
    }

-- 2. VISTA (RENDER)
-- Esta función toma el Estado y devuelve una "Picture" (dibujo)
--------------------------------------------------------------------------------
draw :: GameState -> Picture
draw state = case currentScreen state of
    StartScreen -> drawStartScreen (startBgImage state)
    Menu        -> drawMenuScreen (menuBgImage state) (logoImage state) (selectedOption state)
    Pokedex     -> drawPokedexScreen (menuBgImage state) (logoImage state) (selectedPokemon state)
    PokemonDetail -> drawPokemonScreen (menuBgImage state) (logoImage state) (selectedPokemon state)
    Multiplayer -> drawMultiplayerScreen
    PlayingAI   -> drawAIScreen

-- 3. LOGICA DE TIEMPO (STEP)
-- Se ejecuta X veces por segundo. Por ahora no hace nada.
--------------------------------------------------------------------------------
update :: Float -> GameState -> GameState
update _ state = state

-- 4. MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Cargando recursos..."
    
    -- Cargamos las imágenes
    startBg <- loadBMP "game-client/assets/images/background.bmp"     -- Fondo de pantalla de inicio
    menuBg  <- loadBMP "game-client/assets/images/main_screen.bmp"    -- Fondo del menú principal
    logo    <- loadBMP "game-client/assets/images/logo.bmp"
    
    putStrLn "Iniciando Ventana..."
    
    -- Configuración de la ventana
    let window = InWindow "Pokemonad P2P" (1280, 720) (100, 100)
    
    -- Iniciamos el loop del juego
    play 
        window                              -- Configuración ventana
        black                               -- Color de fondo base
        30                                  -- FPS (cuadros por segundo)
        (initialState startBg menuBg logo)  -- Estado inicial con todas las imágenes
        draw                                -- Función de dibujo
        handleInput                         -- Función de eventos (desde Keys module)
        update                              -- Función de tiempo