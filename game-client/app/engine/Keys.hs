module Engine.Keys 
    ( handleInput
    , Screen(..)
    , GameState(..)
    ) where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.Pure.Game

-- TIPOS DE DATOS
--------------------------------------------------------------------------------
data Screen = StartScreen | Menu | Pokedex | PokemonDetail | Multiplayer | PlayingAI
    deriving (Show, Eq)

data GameState = GameState
    { currentScreen   :: Screen
    , selectedOption  :: Int
    , selectedPokemon :: Int
    , startBgImage    :: Picture
    , menuBgImage     :: Picture
    , logoImage       :: Picture
    }

-- CONTROLADOR (INPUTS)
--------------------------------------------------------------------------------
handleInput :: Event -> GameState -> GameState

-- 1. Navegación (Flechas)
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = max 0 (selectedOption state - 1) }
        Pokedex -> state { selectedPokemon = max 1 (selectedPokemon state - 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = min 2 (selectedOption state + 1) }
        Pokedex -> state { selectedPokemon = min 20 (selectedPokemon state + 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { currentScreen = chooseScreen (selectedOption state) }
        Pokedex -> state { currentScreen = PokemonDetail }
        _    -> state

handleInput (EventKey (SpecialKey KeyBackspace) Down _ _) state = goBack state
handleInput (EventKey (SpecialKey KeyDelete)    Down _ _) state = goBack state
handleInput (EventKey (Char '\b')               Down _ _) state = goBack state

-- 4. Cualquier otra tecla en StartScreen inicia el juego
handleInput (EventKey _ Down _ _) state =
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        _           -> state

-- Ignoramos otros eventos
handleInput _ state = state

--------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES
--------------------------------------------------------------------------------

-- Lógica centralizada para volver atrás
goBack :: GameState -> GameState
goBack state = case currentScreen state of
    StartScreen   -> state
    Menu          -> state
    PokemonDetail -> state { currentScreen = Pokedex }
    Pokedex       -> state { currentScreen = Menu, selectedOption = 0 }
    Multiplayer   -> state { currentScreen = Menu, selectedOption = 0 }
    PlayingAI     -> state { currentScreen = Menu, selectedOption = 0 }

-- Selector de pantallas del menú principal
chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = PlayingAI
chooseScreen _ = Menu