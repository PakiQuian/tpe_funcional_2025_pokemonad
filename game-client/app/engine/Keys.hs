module Engine.Keys 
    ( handleInput
    , Screen(..)
    , GameState(..)
    ) where

import Graphics.Gloss (Picture) -- Necesario para el tipo Picture
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

handleInput (EventKey (SpecialKey KeyBackspace) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state
        Menu -> state
        PokemonDetail -> state { currentScreen = Pokedex }
        Pokedex -> state { currentScreen = Menu, selectedOption = 0 }
        Multiplayer -> state { currentScreen = Menu, selectedOption = 0 }
        PlayingAI -> state { currentScreen = Menu, selectedOption = 0 }

handleInput (EventKey _ Down _ _) state =
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        _           -> state

handleInput _ state = state

chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = PlayingAI
chooseScreen _ = Menu