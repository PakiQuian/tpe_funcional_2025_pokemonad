module Engine.Keys 
    ( handleInput
    , Screen(..)
    , GameState(..)
    ) where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map
import System.Random (StdGen, randomR)

import Game.Pokemon (allPokemon)
import Game.Trainer (Trainer, allTrainers) 

--------------------------------------------------------------------------------
-- TIPOS DE DATOS
--------------------------------------------------------------------------------

data Screen = StartScreen | Menu | Pokedex | PokemonDetail | Multiplayer 
            | TeamSelect | OpponentSelect | BattleScreen
    deriving (Show, Eq)

data GameState = GameState
    { currentScreen       :: Screen
    , selectedOption      :: Int
    , selectedPokemon     :: Int
    , playerTeam          :: [Int]
    , selectedTrainer     :: Maybe Trainer
    , selectedTrainerIndex :: Int
    , startBgImage        :: Picture
    , menuBgImage         :: Picture
    , logoImage           :: Picture
    , pokemonSprites      :: Map.Map Int Picture
    , rngSeed             :: StdGen
    }

--------------------------------------------------------------------------------
-- CONTROLADOR (INPUTS)
--------------------------------------------------------------------------------
handleInput :: Event -> GameState -> GameState

handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = max 0 (selectedOption state - 1) }
        
        Pokedex -> state { selectedPokemon = max 1 (selectedPokemon state - 1) }
        
        TeamSelect -> state { selectedPokemon = max 1 (selectedPokemon state - 1) }
        
        OpponentSelect -> state { selectedTrainerIndex = max 0 (selectedTrainerIndex state - 1) }
        
        _    -> state

handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = min 2 (selectedOption state + 1) }
        
        Pokedex -> state { selectedPokemon = min (length allPokemon) (selectedPokemon state + 1) }
        
        TeamSelect -> state { selectedPokemon = min (length allPokemon) (selectedPokemon state + 1) }
        
        OpponentSelect -> state { selectedTrainerIndex = min (length allTrainers - 1) (selectedTrainerIndex state + 1) }
        
        _    -> state

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { currentScreen = chooseScreen (selectedOption state) }
        Pokedex -> state { currentScreen = PokemonDetail }
        
        OpponentSelect -> handleOpponentSelectEnter state
        
        TeamSelect -> handleTeamSelectEnter state
        
        _    -> state

handleInput (EventKey (SpecialKey KeyBackspace) Down _ _) state = 
    case currentScreen state of
        TeamSelect -> if null (playerTeam state)
                      then goBack state
                      else state { playerTeam = init (playerTeam state) }
        _          -> goBack state

handleInput (EventKey (SpecialKey KeyDelete)    Down _ _) state = 
    case currentScreen state of
        TeamSelect -> if null (playerTeam state)
                      then goBack state
                      else state { playerTeam = init (playerTeam state) }
        _          -> goBack state
    
handleInput (EventKey (Char '\b')               Down _ _) state = 
    case currentScreen state of
        TeamSelect -> if null (playerTeam state)
                      then goBack state
                      else state { playerTeam = init (playerTeam state) }
        _          -> goBack state

handleInput (EventKey (Char 'r') Down _ _) state =
    case currentScreen state of
        TeamSelect -> handleRandomTeam state
        _          -> state

handleInput (EventKey (Char 'R') Down _ _) state =
    case currentScreen state of
        TeamSelect -> handleRandomTeam state
        _          -> state

handleInput (EventKey _ Down _ _) state =
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        _           -> state

handleInput _ state = state

--------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES
--------------------------------------------------------------------------------

goBack :: GameState -> GameState
goBack state = case currentScreen state of
    StartScreen    -> state
    Menu           -> state
    PokemonDetail  -> state { currentScreen = Pokedex }
    Pokedex        -> state { currentScreen = Menu, selectedOption = 0 }
    Multiplayer    -> state { currentScreen = Menu, selectedOption = 0 }
    TeamSelect     -> state { currentScreen = Menu, selectedOption = 0, playerTeam = [] }
    OpponentSelect -> state { currentScreen = TeamSelect }

chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = TeamSelect
chooseScreen _ = Menu

handleTeamSelectEnter :: GameState -> GameState
handleTeamSelectEnter state
    | length (playerTeam state) == 6 = state { currentScreen = OpponentSelect }
    | otherwise = addPokemonToTeam state

addPokemonToTeam :: GameState -> GameState
addPokemonToTeam state
    | pokId `elem` playerTeam state = state
    | length (playerTeam state) >= 6 = state
    | otherwise = state { playerTeam = playerTeam state ++ [pokId] }
  where
    pokId = selectedPokemon state

handleOpponentSelectEnter :: GameState -> GameState
handleOpponentSelectEnter state = 
    let trainer = allTrainers !! selectedTrainerIndex state
    in state 
        { currentScreen = BattleScreen
        , selectedTrainer = Just trainer
        }

handleRandomTeam :: GameState -> GameState
handleRandomTeam state =
    case currentScreen state of
        TeamSelect -> 
            let 
                maxId = length allPokemon
                currentGen = rngSeed state
                (newTeam, nextGen) = generateUniqueRandoms 6 maxId [] currentGen
            in 
                state 
                    { playerTeam = newTeam 
                    , rngSeed = nextGen
                    }
        _ -> state

-- Recibe la cantidad de números a generar, el máximo valor, la lista acumulada y el generador
generateUniqueRandoms :: Int -> Int -> [Int] -> StdGen -> ([Int], StdGen)
generateUniqueRandoms 0 _ acc gen = (acc, gen)
generateUniqueRandoms n maxIdx acc gen = 
    let 
        (r, nextGen) = randomR (1, maxIdx) gen
    in 
        if r `elem` acc 
        then generateUniqueRandoms n maxIdx acc nextGen
        else generateUniqueRandoms (n-1) maxIdx (r:acc) nextGen