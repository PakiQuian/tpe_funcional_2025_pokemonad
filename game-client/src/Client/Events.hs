module Client.Events
  ( handleWorldInput,
    handleWorldTick,
  )
where

import Client.Handlers.AISimulatorHandler (launchAITrainingIfRequested, mergeAITraining)
import Client.Handlers.BattleHandler
  ( handleBattleBack,
    handleBattleDown,
    handleBattleEnter,
    handleBattleLeft,
    handleBattleRight,
    handleBattleUp,
    returnToMainMenu,
  )
import Client.Handlers.MultiplayerHandler (handleMultiplayerChar, handleMultiplayerEnter, startMultiplayerNet)
import Client.Handlers.OpponentSelectHandler (handleOpponentSelectEnter)
import Client.Handlers.TeamSelectHandler (handleRandomTeam, handleTeamSelectEnter)
import Client.State (GameState (..), World (..), disconnectNetWorld, drainNetInbox, mergeNetAsync)
import Client.Types (Screen (..))
import Graphics.Gloss.Interface.Pure.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down, Up),
    SpecialKey (KeyBackspace, KeyDelete, KeyDown, KeyEnter, KeyEsc, KeyLeft, KeyRight, KeyUp),
  )
import Pokemonad.Core.Pokemon (allPokemon)
import Pokemonad.Core.Trainer (allTrainers)
import Pokemonad.Core.Types (PokemonId (..))

handleWorldInput :: Event -> World -> IO World
handleWorldInput ev w = do
  let g0 = worldGame w
  w1 <- launchAITrainingIfRequested ev w
  let g1 = handleGameInput ev (worldGame w1)
      leftMP = currentScreen g0 == Multiplayer && currentScreen g1 /= Multiplayer
  w2 <- if leftMP then disconnectNetWorld w1 else pure w1
  let w3 = w2 {worldGame = g1}
  case multiplayerPending g1 of
    Nothing -> pure w3
    Just intent ->
      let g2 = g1 {multiplayerPending = Nothing}
       in startMultiplayerNet intent (w3 {worldGame = g2})

handleWorldTick :: Float -> World -> IO World
handleWorldTick dt w = do
  wMerged <- mergeNetAsync w
  wDrained <- drainNetInbox wMerged
  wAI <- mergeAITraining wDrained
  pure wAI {worldGame = handleTick dt (worldGame wAI)}

handleGameInput :: Event -> GameState -> GameState
handleGameInput ev gs = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> moveUp (gs {holdingUp = True, scrollTimer = -0.3})
  EventKey (SpecialKey KeyUp) Up _ _ -> gs {holdingUp = False}
  EventKey (SpecialKey KeyDown) Down _ _ -> moveDown (gs {holdingDown = True, scrollTimer = -0.3})
  EventKey (SpecialKey KeyDown) Up _ _ -> gs {holdingDown = False}
  EventKey (SpecialKey KeyLeft) Down _ _ -> handleLeft gs
  EventKey (SpecialKey KeyRight) Down _ _ -> handleRight gs
  EventKey (SpecialKey KeyEnter) Down _ _ -> handleEnter gs
  EventKey (SpecialKey KeyBackspace) Down _ _ -> handleBackKey gs
  EventKey (SpecialKey KeyDelete) Down _ _ -> handleBackKey gs
  EventKey (Char '\b') Down _ _ -> handleBackKey gs
  EventKey (SpecialKey KeyEsc) Down _ _ -> handleBackKey gs
  EventKey (Char 'r') Down _ _ -> handleCharR gs
  EventKey (Char 'R') Down _ _ -> handleCharR gs
  EventKey (Char c) Down _ _
    | currentScreen gs == Multiplayer -> handleMultiplayerChar c gs
  EventKey _ Down _ _ -> case currentScreen gs of
    StartScreen -> gs {currentScreen = Menu}
    _ -> gs
  _ -> gs

handleTick :: Float -> GameState -> GameState
handleTick dt state =
  let scrollSpeed = 0.05
      newTimer = scrollTimer state + dt
   in if holdingUp state && newTimer >= scrollSpeed
        then moveUp (state {scrollTimer = 0})
        else
          if holdingDown state && newTimer >= scrollSpeed
            then moveDown (state {scrollTimer = 0})
            else state {scrollTimer = newTimer}

moveUp :: GameState -> GameState
moveUp state = case currentScreen state of
  Menu -> state {selectedOption = max 0 (selectedOption state - 1)}
  Multiplayer -> state {multiplayerRow = max 0 (multiplayerRow state - 1)}
  Pokedex -> state {selectedPokemonId = PokemonId (max 1 (unPokemonId (selectedPokemonId state) - 1))}
  TeamSelect -> state {selectedPokemonId = PokemonId (max 1 (unPokemonId (selectedPokemonId state) - 1))}
  OpponentSelect -> state {selectedTrainerIndex = max 0 (selectedTrainerIndex state - 1)}
  BattleScreen -> handleBattleUp state
  _ -> state

moveDown :: GameState -> GameState
moveDown state = case currentScreen state of
  Menu -> state {selectedOption = min 3 (selectedOption state + 1)}
  Multiplayer -> state {multiplayerRow = min 3 (multiplayerRow state + 1)}
  Pokedex -> state {selectedPokemonId = PokemonId (min (length allPokemon) (unPokemonId (selectedPokemonId state) + 1))}
  TeamSelect -> state {selectedPokemonId = PokemonId (min (length allPokemon) (unPokemonId (selectedPokemonId state) + 1))}
  OpponentSelect -> state {selectedTrainerIndex = min (length allTrainers - 1) (selectedTrainerIndex state + 1)}
  BattleScreen -> handleBattleDown state
  _ -> state

handleLeft :: GameState -> GameState
handleLeft state = case currentScreen state of
  BattleScreen -> handleBattleLeft state
  _ -> state

handleRight :: GameState -> GameState
handleRight state = case currentScreen state of
  BattleScreen -> handleBattleRight state
  _ -> state

handleEnter :: GameState -> GameState
handleEnter state = case currentScreen state of
  StartScreen -> state {currentScreen = Menu}
  Menu -> state {currentScreen = chooseScreen (selectedOption state)}
  AISimulator -> state
  Multiplayer -> handleMultiplayerEnter state
  Pokedex -> state {currentScreen = PokemonDetail}
  OpponentSelect -> handleOpponentSelectEnter state
  TeamSelect -> handleTeamSelectEnter state
  BattleScreen -> handleBattleEnter state
  BattleResultScreen -> returnToMainMenu state
  _ -> state

handleBackKey :: GameState -> GameState
handleBackKey state = case currentScreen state of
  Multiplayer ->
    case multiplayerRow state of
      0 ->
        if null (multiplayerHost state)
          then goBack state
          else state {multiplayerHost = init (multiplayerHost state)}
      1 ->
        if null (multiplayerPort state)
          then state {multiplayerRow = 0}
          else state {multiplayerPort = init (multiplayerPort state)}
      _ -> goBack state
  TeamSelect ->
    if null (playerTeam state)
      then goBack state
      else state {playerTeam = init (playerTeam state)}
  BattleScreen -> handleBattleBack state
  _ -> goBack state

goBack :: GameState -> GameState
goBack state = case currentScreen state of
  StartScreen -> state
  Menu -> state
  PokemonDetail -> state {currentScreen = Pokedex}
  Pokedex -> state {currentScreen = Menu, selectedOption = 0}
  Multiplayer ->
    state
      { currentScreen = Menu,
        selectedOption = 0,
        multiplayerError = Nothing,
        multiplayerPending = Nothing
      }
  AISimulator -> state {currentScreen = Menu, selectedOption = 0}
  TeamSelect -> state {currentScreen = Menu, selectedOption = 0, playerTeam = []}
  OpponentSelect -> state {currentScreen = TeamSelect}
  _ -> state

handleCharR :: GameState -> GameState
handleCharR state = case currentScreen state of
  TeamSelect -> handleRandomTeam state
  _ -> state

chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = AISimulator
chooseScreen 3 = TeamSelect
chooseScreen _ = Menu
