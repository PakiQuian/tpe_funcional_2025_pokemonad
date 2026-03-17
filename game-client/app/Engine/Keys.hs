module Engine.Keys
  ( handleInput,
    handleTick,
    Screen (..),
    GameState (..),
  )
where

import qualified Data.Map as Map
import Engine.GameState (BattleMenuType (..), GameState (..), Screen (..))
import Game.Battle (BattleState, initBattle)
import Game.Pokemon (allPokemon)
import Game.Trainer (Trainer, allTrainers)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.Pure.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down, Up),
    Picture,
    SpecialKey (KeyBackspace, KeyDelete, KeyDown, KeyEnter, KeyLeft, KeyRight, KeyUp),
  )
import System.Random (StdGen, randomR)

-- ==============================================================================
-- CONTROLADOR (INPUTS)
-- ==============================================================================
handleInput :: Event -> GameState -> GameState
-- Key Up
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state =
  moveUp (state {holdingUp = True, scrollTimer = -0.3})
handleInput (EventKey (SpecialKey KeyUp) Up _ _) state =
  state {holdingUp = False}
-- Key Down
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =
  moveDown (state {holdingDown = True, scrollTimer = -0.3})
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state =
  state {holdingDown = False}
-- Key Left
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state =
  case currentScreen state of
    BattleScreen ->
      case battleMenuType state of
        MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if odd c then c - 1 else c}
        FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if odd c then c - 1 else c}
        QuitConfirmMenu -> state {battleMoveIndex = 0}
    _ -> state
-- Key Right
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state =
  case currentScreen state of
    BattleScreen ->
      case battleMenuType state of
        MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if even c then c + 1 else c}
        FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if even c then c + 1 else c}
        QuitConfirmMenu -> state {battleMoveIndex = 1}
    _ -> state
-- Key Enter
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state =
  case currentScreen state of
    StartScreen -> state {currentScreen = Menu}
    Menu -> state {currentScreen = chooseScreen (selectedOption state)}
    Pokedex -> state {currentScreen = PokemonDetail}
    OpponentSelect -> handleOpponentSelectEnter state
    TeamSelect -> handleTeamSelectEnter state
    BattleScreen ->
      case battleMenuType state of
        MainBattleMenu ->
          case battleMenuIndex state of
            0 -> state {battleMenuType = FightMenu, battleMoveIndex = 0}
            3 -> state {battleMenuType = QuitConfirmMenu, battleMoveIndex = 1}
            _ -> state
        FightMenu ->
          state -- Aquí programaremos el ataque más adelante
        BagMenu -> state -- Aquí programaremos la lógica de la bolsa más adelante
        PokemonMenu -> state -- Aquí programaremos la lógica de cambio de Pokémon más adelante
        QuitConfirmMenu ->
          case battleMoveIndex state of
            0 -> state {currentScreen = Menu, battleMenuType = MainBattleMenu, battleState = Nothing, battleMoveIndex = 0, battleMenuIndex = 0, selectedTrainer = Nothing, selectedTrainerIndex = 0, playerTeam = []}
            1 -> state {battleMenuType = MainBattleMenu}
            _ -> state
    _ -> state
-- Key Backspace / Delete
handleInput (EventKey (SpecialKey KeyBackspace) Down _ _) state = handleBackKey state
handleInput (EventKey (SpecialKey KeyDelete) Down _ _) state = handleBackKey state
handleInput (EventKey (Char '\b') Down _ _) state = handleBackKey state
-- Key 'r' / 'R'
handleInput (EventKey (Char 'r') Down _ _) state =
  case currentScreen state of
    TeamSelect -> handleRandomTeam state
    _ -> state
handleInput (EventKey (Char 'R') Down _ _) state =
  case currentScreen state of
    TeamSelect -> handleRandomTeam state
    _ -> state
-- Cualquier otra tecla
handleInput (EventKey _ Down _ _) state =
  case currentScreen state of
    StartScreen -> state {currentScreen = Menu}
    _ -> state
handleInput _ state = state

--------------------------------------------------------------------------------
-- LOGICA DE SCROLL CONTINUO (TICK)
--------------------------------------------------------------------------------

handleTick :: Float -> GameState -> GameState
handleTick dt state =
  let -- Velocidad de scroll continuo (0.05 segundos = 20 pokemons por segundo)
      scrollSpeed = 0.05
      newTimer = scrollTimer state + dt
   in if holdingUp state && newTimer >= scrollSpeed
        then moveUp (state {scrollTimer = 0}) -- Scrollea y resetea timer a 0
        else
          if holdingDown state && newTimer >= scrollSpeed
            then moveDown (state {scrollTimer = 0}) -- Scrollea y resetea timer a 0
            else state {scrollTimer = newTimer} -- Solo suma tiempo

--------------------------------------------------------------------------------
-- FUNCIONES DE MOVIMIENTO EXTRAIDAS
--------------------------------------------------------------------------------

moveUp :: GameState -> GameState
moveUp state = case currentScreen state of
  StartScreen -> state {currentScreen = Menu}
  Menu -> state {selectedOption = max 0 (selectedOption state - 1)}
  Pokedex -> state {selectedPokemon = max 1 (selectedPokemon state - 1)}
  TeamSelect -> state {selectedPokemon = max 1 (selectedPokemon state - 1)}
  OpponentSelect -> state {selectedTrainerIndex = max 0 (selectedTrainerIndex state - 1)}
  BattleScreen ->
    if battleMenuType state == MainBattleMenu
      then let c = battleMenuIndex state in state {battleMenuIndex = if c >= 2 then c - 2 else c}
      else let c = battleMoveIndex state in state {battleMoveIndex = if c >= 2 then c - 2 else c}
  _ -> state

moveDown :: GameState -> GameState
moveDown state = case currentScreen state of
  StartScreen -> state {currentScreen = Menu}
  Menu -> state {selectedOption = min 2 (selectedOption state + 1)}
  Pokedex -> state {selectedPokemon = min (length allPokemon) (selectedPokemon state + 1)}
  TeamSelect -> state {selectedPokemon = min (length allPokemon) (selectedPokemon state + 1)}
  OpponentSelect -> state {selectedTrainerIndex = min (length allTrainers - 1) (selectedTrainerIndex state + 1)}
  BattleScreen ->
    if battleMenuType state == MainBattleMenu
      then let c = battleMenuIndex state in state {battleMenuIndex = if c <= 1 then c + 2 else c}
      else let c = battleMoveIndex state in state {battleMoveIndex = if c <= 1 then c + 2 else c}
  _ -> state

--------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES
--------------------------------------------------------------------------------

handleBackKey :: GameState -> GameState
handleBackKey state = case currentScreen state of
  TeamSelect ->
    if null (playerTeam state)
      then goBack state
      else state {playerTeam = init (playerTeam state)}
  BattleScreen ->
    case battleMenuType state of
      FightMenu -> state {battleMenuType = MainBattleMenu}
      BagMenu -> state {battleMenuType = MainBattleMenu}
      PokemonMenu -> state {battleMenuType = MainBattleMenu}
      QuitConfirmMenu -> state {battleMenuType = MainBattleMenu}
      _ -> state
  _ -> goBack state

goBack :: GameState -> GameState
goBack state = case currentScreen state of
  StartScreen -> state
  Menu -> state
  PokemonDetail -> state {currentScreen = Pokedex}
  Pokedex -> state {currentScreen = Menu, selectedOption = 0}
  Multiplayer -> state {currentScreen = Menu, selectedOption = 0}
  TeamSelect -> state {currentScreen = Menu, selectedOption = 0, playerTeam = []}
  OpponentSelect -> state {currentScreen = TeamSelect}
  BattleScreen -> state

chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = TeamSelect
chooseScreen _ = Menu

handleTeamSelectEnter :: GameState -> GameState
handleTeamSelectEnter state
  | length (playerTeam state) == 6 = state {currentScreen = OpponentSelect}
  | otherwise = addPokemonToTeam state

addPokemonToTeam :: GameState -> GameState
addPokemonToTeam state
  | pokId `elem` playerTeam state = state
  | length (playerTeam state) >= 6 = state
  | otherwise = state {playerTeam = playerTeam state ++ [pokId]}
  where
    pokId = selectedPokemon state

handleOpponentSelectEnter :: GameState -> GameState
handleOpponentSelectEnter state =
  let trainer = allTrainers !! selectedTrainerIndex state
      myTeam = playerTeam state

      newBattle = initBattle myTeam trainer

      bgCount = length (battleBackgrounds state)
      (randIndex, nextRng) =
        if bgCount > 0
          then generateUniqueRandom bgCount (rngSeed state)
          else (0, rngSeed state)
   in state
        { currentScreen = BattleScreen,
          selectedTrainer = Just trainer,
          battleState = Just newBattle,
          currentBattleBg = randIndex,
          rngSeed = nextRng
        }

handleRandomTeam :: GameState -> GameState
handleRandomTeam state =
  case currentScreen state of
    TeamSelect ->
      let maxId = length allPokemon
          currentGen = rngSeed state
          (newTeam, nextGen) = generateUniqueRandoms 6 maxId [] currentGen
       in state
            { playerTeam = newTeam,
              rngSeed = nextGen
            }
    _ -> state

-- Recibe la cantidad de números a generar, el máximo valor, la lista acumulada y el generador
generateUniqueRandoms :: Int -> Int -> [Int] -> StdGen -> ([Int], StdGen)
generateUniqueRandoms 0 _ acc gen = (acc, gen)
generateUniqueRandoms n maxIdx acc gen =
  let (r, nextGen) = randomR (1, maxIdx) gen
   in if r `elem` acc
        then generateUniqueRandoms n maxIdx acc nextGen
        else generateUniqueRandoms (n - 1) maxIdx (r : acc) nextGen

-- Recibe maximo numero a generar y el generador, devuelve un número único y el generador
generateUniqueRandom :: Int -> StdGen -> (Int, StdGen)
generateUniqueRandom maxIdx gen =
  let (r, nextGen) = randomR (1, maxIdx) gen
   in (r, nextGen)