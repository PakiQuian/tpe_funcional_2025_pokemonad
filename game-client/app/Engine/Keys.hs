module Engine.Keys
  ( handleInput,
    handleTick,
    Screen (..),
    GameState (..),
  )
where

import Data.Char (isAlphaNum, isDigit)
import qualified Data.Map as Map
import Engine.GameState (BattleMenuType (..), GameState (..), MultiplayerIntent (..), Screen (..))
import Game.Battle (BattlePokemon (bpStatus), bpHp)
import Game.Engine (BattleAction (..), BattlePhase (..), BattleState (..), initBattleEngine, submitPlayerAction)
import Game.Pokemon (allPokemon)
import Game.Trainer (Trainer, allTrainers)
import Game.Types (Status (..))
import Graphics.Gloss.Interface.Pure.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down, Up),
    Picture,
    SpecialKey (KeyBackspace, KeyDelete, KeyDown, KeyEnter, KeyEsc, KeyLeft, KeyRight, KeyUp),
  )
import Network.Socket (PortNumber)
import System.Random (StdGen, randomR)
import Text.Read (readMaybe)

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
      if isForcedSwitchPhase state
        then case battleMenuType state of
          PokemonMenu ->
            let c = battleBenchIndex state
             in state {battleBenchIndex = previousSwitchableBenchIndex state c}
          SwitchConfirmMenu -> state {battleMoveIndex = 0}
          _ -> state
        else case battleMenuType state of
          MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if odd c then c - 1 else c}
          FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if odd c then c - 1 else c}
          PokemonMenu ->
            let c = battleBenchIndex state
             in state {battleBenchIndex = previousSwitchableBenchIndex state c}
          SwitchConfirmMenu -> state {battleMoveIndex = 0}
          QuitConfirmMenu -> state {battleMoveIndex = 0}
    _ -> state -- Key Right
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state =
  case currentScreen state of
    BattleScreen ->
      if isForcedSwitchPhase state
        then case battleMenuType state of
          PokemonMenu ->
            let c = battleBenchIndex state
             in state {battleBenchIndex = nextSwitchableBenchIndex state c}
          SwitchConfirmMenu -> state {battleMoveIndex = 1}
          _ -> state
        else case battleMenuType state of
          MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if even c then c + 1 else c}
          FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if even c then c + 1 else c}
          PokemonMenu ->
            let c = battleBenchIndex state
             in state {battleBenchIndex = nextSwitchableBenchIndex state c}
          SwitchConfirmMenu -> state {battleMoveIndex = 1}
          QuitConfirmMenu -> state {battleMoveIndex = 1}
    _ -> state
-- Key Enter
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state =
  case currentScreen state of
    StartScreen -> state {currentScreen = Menu}
    Menu -> state {currentScreen = chooseScreen (selectedOption state)}
    Multiplayer -> handleMultiplayerEnter state
    Pokedex -> state {currentScreen = PokemonDetail}
    OpponentSelect -> handleOpponentSelectEnter state
    TeamSelect -> handleTeamSelectEnter state
    BattleScreen ->
      if isForcedSwitchPhase state
        then case battleMenuType state of
          PokemonMenu ->
            case battleState state of
              Just bState ->
                if battleBenchIndex state < length (playerBench bState)
                  then state {battleMenuType = SwitchConfirmMenu, battleMoveIndex = 0}
                  else state
              Nothing -> state
          SwitchConfirmMenu ->
            case battleMoveIndex state of
              0 -> submitSelectedSwitch state
              1 -> state {battleMenuType = PokemonMenu}
              _ -> state
          _ ->
            let firstIdx = firstSwitchableBenchIndex state
             in state {battleMenuType = PokemonMenu, battleMoveIndex = 0, battleBenchIndex = firstIdx}
        else case battleMenuType state of
          MainBattleMenu ->
            case battleMenuIndex state of
              0 -> state {battleMenuType = FightMenu, battleMoveIndex = 0}
              2 ->
                let firstIdx = firstSwitchableBenchIndex state
                 in state {battleMenuType = PokemonMenu, battleBenchIndex = firstIdx}
              3 -> state {battleMenuType = QuitConfirmMenu, battleMoveIndex = 1}
              _ -> state
          FightMenu ->
            submitSelectedMove state
          BagMenu -> state -- Aquí programaremos la lógica de la bolsa más adelante
          PokemonMenu ->
            case battleState state of
              Just bState ->
                if battleBenchIndex state < length (playerBench bState)
                  then state {battleMenuType = SwitchConfirmMenu, battleMoveIndex = 0}
                  else state
              Nothing -> state
          SwitchConfirmMenu ->
            case battleMoveIndex state of
              0 -> submitSelectedSwitch state
              1 ->
                state {battleMenuType = PokemonMenu}
              _ -> state
          QuitConfirmMenu ->
            case battleMoveIndex state of
              0 -> state {currentScreen = Menu, battleMenuType = MainBattleMenu, battleState = Nothing, battleMoveIndex = 0, battleMenuIndex = 0, selectedTrainer = Nothing, selectedTrainerIndex = 0, playerTeam = []}
              1 -> state {battleMenuType = MainBattleMenu}
              _ -> state
    BattleResultScreen -> returnToMainMenu state
    _ -> state
-- Key Backspace / Delete / ESC
handleInput (EventKey (SpecialKey KeyBackspace) Down _ _) state = handleBackKey state
handleInput (EventKey (SpecialKey KeyDelete) Down _ _) state = handleBackKey state
handleInput (EventKey (Char '\b') Down _ _) state = handleBackKey state
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) state = handleBackKey state
-- Key 'r' / 'R'
handleInput (EventKey (Char 'r') Down _ _) state =
  case currentScreen state of
    TeamSelect -> handleRandomTeam state
    _ -> state
handleInput (EventKey (Char 'R') Down _ _) state =
  case currentScreen state of
    TeamSelect -> handleRandomTeam state
    _ -> state
-- Texto en pantalla multijugador
handleInput (EventKey (Char c) Down _ _) state
  | currentScreen state == Multiplayer = handleMultiplayerChar c state
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
  Multiplayer -> state {multiplayerRow = max 0 (multiplayerRow state - 1)}
  Pokedex -> state {selectedPokemon = max 1 (selectedPokemon state - 1)}
  TeamSelect -> state {selectedPokemon = max 1 (selectedPokemon state - 1)}
  OpponentSelect -> state {selectedTrainerIndex = max 0 (selectedTrainerIndex state - 1)}
  BattleScreen ->
    case battleMenuType state of
      MainBattleMenu ->
        let c = battleMenuIndex state
         in state {battleMenuIndex = if c >= 2 then c - 2 else c}
      FightMenu ->
        let c = battleMoveIndex state
         in state {battleMoveIndex = if c >= 2 then c - 2 else c}
      PokemonMenu ->
        let c = battleBenchIndex state
         in state {battleBenchIndex = previousSwitchableBenchIndex state c}
      _ -> state
  _ -> state

moveDown :: GameState -> GameState
moveDown state = case currentScreen state of
  StartScreen -> state {currentScreen = Menu}
  Menu -> state {selectedOption = min 2 (selectedOption state + 1)}
  Multiplayer -> state {multiplayerRow = min 3 (multiplayerRow state + 1)}
  Pokedex -> state {selectedPokemon = min (length allPokemon) (selectedPokemon state + 1)}
  TeamSelect -> state {selectedPokemon = min (length allPokemon) (selectedPokemon state + 1)}
  OpponentSelect -> state {selectedTrainerIndex = min (length allTrainers - 1) (selectedTrainerIndex state + 1)}
  BattleScreen ->
    case battleMenuType state of
      MainBattleMenu ->
        let c = battleMenuIndex state
         in state {battleMenuIndex = if c <= 1 then c + 2 else c}
      FightMenu ->
        let c = battleMoveIndex state
         in state {battleMoveIndex = if c <= 1 then c + 2 else c}
      PokemonMenu ->
        let c = battleBenchIndex state
         in state {battleBenchIndex = nextSwitchableBenchIndex state c}
      _ -> state
  _ -> state

benchLeft :: Int -> Int
benchLeft 1 = 0
benchLeft 3 = 2
benchLeft i = i

benchRight :: Int -> Int -> Int
benchRight benchLen 0
  | benchLen >= 2 = 1
benchRight benchLen 2
  | benchLen >= 4 = 3
benchRight _ i = i

benchUp :: Int -> Int
benchUp 4 = 2
benchUp 3 = 1
benchUp 2 = 0
benchUp i = i

benchDown :: Int -> Int -> Int
benchDown benchLen 0
  | benchLen >= 3 = 2
benchDown benchLen 1
  | benchLen >= 4 = 3
benchDown benchLen 2
  | benchLen >= 5 = 4
benchDown _ i = i

--------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES
--------------------------------------------------------------------------------

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
  BattleScreen ->
    if isForcedSwitchPhase state
      then case battleMenuType state of
        SwitchConfirmMenu -> state {battleMenuType = PokemonMenu, battleMoveIndex = 0}
        _ -> state
      else case battleMenuType state of
        FightMenu -> state {battleMenuType = MainBattleMenu}
        BagMenu -> state {battleMenuType = MainBattleMenu}
        PokemonMenu -> state {battleMenuType = MainBattleMenu}
        SwitchConfirmMenu -> state {battleMenuType = PokemonMenu, battleMoveIndex = 0}
        QuitConfirmMenu -> state {battleMenuType = MainBattleMenu}
        _ -> state
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
      newBattle = initBattleEngine myTeam trainer

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
          rngSeed = nextRng,
          battleMenuType = MainBattleMenu,
          battleBenchIndex = firstSwitchableBenchIndexFromBattle newBattle
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

submitSelectedMove :: GameState -> GameState
submitSelectedMove state =
  case battleState state of
    Nothing -> state
    Just bState ->
      let action = ActionMove (battleMoveIndex state)
          (nextBattle, nextRng) = submitPlayerAction (rngSeed state) bState action
          resultScreen = battleResultScreenFrom nextBattle
       in state
            { battleState = Just nextBattle,
              rngSeed = nextRng,
              currentScreen = resultScreen,
              battleMenuType = nextBattleMenuType nextBattle,
              battleMoveIndex = 0,
              battleBenchIndex = firstSwitchableBenchIndexFromBattle nextBattle
            }

submitSelectedSwitch :: GameState -> GameState
submitSelectedSwitch state =
  case battleState state of
    Nothing -> state
    Just bState ->
      let action = ActionSwitch (battleBenchIndex state)
          (nextBattle, nextRng) = submitPlayerAction (rngSeed state) bState action
          resultScreen = battleResultScreenFrom nextBattle
       in state
            { battleState = Just nextBattle,
              rngSeed = nextRng,
              currentScreen = resultScreen,
              battleMenuType = nextBattleMenuType nextBattle,
              battleMoveIndex = 0,
              battleBenchIndex = firstSwitchableBenchIndexFromBattle nextBattle
            }

isForcedSwitchPhase :: GameState -> Bool
isForcedSwitchPhase state =
  case battleState state of
    Just bState -> phase bState == WaitingForForcedPlayerSwitch
    Nothing -> False

nextBattleMenuType :: BattleState -> BattleMenuType
nextBattleMenuType bState
  | phase bState == WaitingForForcedPlayerSwitch = PokemonMenu
  | otherwise = MainBattleMenu

battleResultScreenFrom :: BattleState -> Screen
battleResultScreenFrom bState =
  case phase bState of
    BattleEnded _ -> BattleResultScreen
    _ -> BattleScreen

returnToMainMenu :: GameState -> GameState
returnToMainMenu state =
  state
    { currentScreen = Menu,
      battleState = Nothing,
      battleMenuType = MainBattleMenu,
      battleMenuIndex = 0,
      battleMoveIndex = 0,
      battleBenchIndex = 0,
      selectedTrainer = Nothing,
      selectedTrainerIndex = 0,
      playerTeam = []
    }

firstSwitchableBenchIndex :: GameState -> Int
firstSwitchableBenchIndex state = maybe 0 firstSwitchableBenchIndexFromBattle (battleState state)

firstSwitchableBenchIndexFromBattle :: BattleState -> Int
firstSwitchableBenchIndexFromBattle bState =
  case switchableBenchIndices bState of
    (x : _) -> x
    [] -> 0

switchableBenchIndices :: BattleState -> [Int]
switchableBenchIndices bState =
  [idx | (idx, bp) <- zip [0 ..] (playerBench bState), bpStatus bp /= Fainted]

nextSwitchableBenchIndex :: GameState -> Int -> Int
nextSwitchableBenchIndex state current =
  case battleState state of
    Nothing -> current
    Just bState ->
      let valid = switchableBenchIndices bState
       in case valid of
            [] -> 0
            _ -> case filter (> current) valid of
              (x : _) -> x
              [] -> safeHead valid

safeHead :: [a] -> a
safeHead (x : _) = x
safeHead [] = error "safeHead: empty list"

safeLast :: [a] -> a
safeLast xs = case reverse xs of
  (x : _) -> x
  [] -> error "safeLast: empty list"

previousSwitchableBenchIndex :: GameState -> Int -> Int
previousSwitchableBenchIndex state current =
  case battleState state of
    Nothing -> current
    Just bState ->
      let valid = switchableBenchIndices bState
       in case valid of
            [] -> 0
            _ -> case filter (< current) valid of
              [] -> safeLast valid
              xs -> safeLast xs

--------------------------------------------------------------------------------
-- MULTIJUGADOR P2P (campos host/puerto y acciones)
--------------------------------------------------------------------------------

parsePortStr :: String -> Maybe PortNumber
parsePortStr s
  | null s = Nothing
  | otherwise = case readMaybe s :: Maybe Int of
      Just n | n >= 1 && n <= 65535 -> Just (fromIntegral n :: PortNumber)
      _ -> Nothing

handleMultiplayerChar :: Char -> GameState -> GameState
handleMultiplayerChar c state =
  case multiplayerRow state of
    0 ->
      if isHostChar c
        then state {multiplayerHost = multiplayerHost state ++ [c], multiplayerError = Nothing}
        else state
    1 ->
      if isDigit c
        then state {multiplayerPort = multiplayerPort state ++ [c], multiplayerError = Nothing}
        else state
    _ -> state

isHostChar :: Char -> Bool
isHostChar c = isAlphaNum c || c `elem` (".-_" :: String)

handleMultiplayerEnter :: GameState -> GameState
handleMultiplayerEnter state =
  case multiplayerRow state of
    0 -> state {multiplayerRow = 1}
    1 -> state {multiplayerRow = 2}
    2 ->
      case parsePortStr (multiplayerPort state) of
        Nothing -> state {multiplayerError = Just "Puerto invalido (1-65535)."}
        Just p -> state {multiplayerPending = Just (MPListen p), multiplayerError = Nothing}
    3 ->
      case parsePortStr (multiplayerPort state) of
        Nothing -> state {multiplayerError = Just "Puerto invalido (1-65535)."}
        Just p ->
          let h = multiplayerHost state
           in if null h
                then state {multiplayerError = Just "Indica un host."}
                else state {multiplayerPending = Just (MPConnect h p), multiplayerError = Nothing}
    _ -> state
