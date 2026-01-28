module Engine.Keys 
    ( handleInput
    , Screen(..)
    , GameState(..)
    ) where

import Graphics.Gloss.Interface.Pure.Game

-- TIPOS DE DATOS
--------------------------------------------------------------------------------
-- Definimos las pantallas posibles
data Screen = StartScreen | Menu | Pokedex | Multiplayer | PlayingAI
    deriving (Show, Eq)

data GameState = GameState
    { currentScreen  :: Screen
    , selectedOption :: Int
    , startBgImage   :: Picture
    , menuBgImage    :: Picture
    }

-- CONTROLADOR (INPUTS)
-- Reacciona a eventos de teclado
--------------------------------------------------------------------------------
handleInput :: Event -> GameState -> GameState
-- Teclas específicas primero
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = max 0 (selectedOption state - 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = min 2 (selectedOption state + 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { currentScreen = chooseScreen (selectedOption state) }
        _    -> state

handleInput (EventKey (SpecialKey KeyEsc) Down _ _) state = 
    -- ESC vuelve al menú (excepto desde StartScreen que pasa al menú también)
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        _           -> state { currentScreen = Menu }

-- Cualquier otra tecla en StartScreen pasa al menú principal
handleInput (EventKey _ Down _ _) state =
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        _           -> state

-- Ignoramos cualquier otro evento (mouse, soltar teclas, etc.)
handleInput _ state = state

-- Helper para convertir índice a pantalla
chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = PlayingAI
chooseScreen _ = Menu
