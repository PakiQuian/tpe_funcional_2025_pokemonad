module Client.Handlers.BattleHandler
  ( handleBattleUp,
    handleBattleDown,
    handleBattleLeft,
    handleBattleRight,
    handleBattleEnter,
    handleBattleBack,
    isForcedSwitchPhase,
    firstSwitchableBenchIndex,
    firstSwitchableBenchIndexFromBattle,
    nextSwitchableBenchIndex,
    previousSwitchableBenchIndex,
    nextBattleMenuType,
    battleResultScreenFrom,
    returnToMainMenu,
  )
where

import Client.State (GameState (..))
import Client.Types (BattleMenuType (..), Screen (..))
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePokemon (..),
    BattlePhase (..),
    BattleState (..),
  )
import Pokemonad.Battle.Turn (submitPlayerActionWithEnemyWeights)
import Pokemonad.Core.Types (Status (..))

handleBattleUp :: GameState -> GameState
handleBattleUp state =
  if isForcedSwitchPhase state
    then case battleMenuType state of
      PokemonMenu -> state {battleBenchIndex = previousSwitchableBenchIndex state (battleBenchIndex state)}
      _ -> state
    else case battleMenuType state of
      MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if c >= 2 then c - 2 else c}
      FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if c >= 2 then c - 2 else c}
      PokemonMenu -> state {battleBenchIndex = previousSwitchableBenchIndex state (battleBenchIndex state)}
      _ -> state

handleBattleDown :: GameState -> GameState
handleBattleDown state =
  if isForcedSwitchPhase state
    then case battleMenuType state of
      PokemonMenu -> state {battleBenchIndex = nextSwitchableBenchIndex state (battleBenchIndex state)}
      _ -> state
    else case battleMenuType state of
      MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if c <= 1 then c + 2 else c}
      FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if c <= 1 then c + 2 else c}
      PokemonMenu -> state {battleBenchIndex = nextSwitchableBenchIndex state (battleBenchIndex state)}
      _ -> state

handleBattleLeft :: GameState -> GameState
handleBattleLeft state =
  if isForcedSwitchPhase state
    then case battleMenuType state of
      PokemonMenu -> state {battleBenchIndex = previousSwitchableBenchIndex state (battleBenchIndex state)}
      SwitchConfirmMenu -> state {battleMoveIndex = 0}
      _ -> state
    else case battleMenuType state of
      MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if odd c then c - 1 else c}
      FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if odd c then c - 1 else c}
      PokemonMenu -> state {battleBenchIndex = previousSwitchableBenchIndex state (battleBenchIndex state)}
      SwitchConfirmMenu -> state {battleMoveIndex = 0}
      QuitConfirmMenu -> state {battleMoveIndex = 0}
      _ -> state

handleBattleRight :: GameState -> GameState
handleBattleRight state =
  if isForcedSwitchPhase state
    then case battleMenuType state of
      PokemonMenu -> state {battleBenchIndex = nextSwitchableBenchIndex state (battleBenchIndex state)}
      SwitchConfirmMenu -> state {battleMoveIndex = 1}
      _ -> state
    else case battleMenuType state of
      MainBattleMenu -> let c = battleMenuIndex state in state {battleMenuIndex = if even c then c + 1 else c}
      FightMenu -> let c = battleMoveIndex state in state {battleMoveIndex = if even c then c + 1 else c}
      PokemonMenu -> state {battleBenchIndex = nextSwitchableBenchIndex state (battleBenchIndex state)}
      SwitchConfirmMenu -> state {battleMoveIndex = 1}
      QuitConfirmMenu -> state {battleMoveIndex = 1}
      _ -> state

handleBattleEnter :: GameState -> GameState
handleBattleEnter state =
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
        state {battleMenuType = PokemonMenu, battleMoveIndex = 0, battleBenchIndex = firstSwitchableBenchIndex state}
    else case battleMenuType state of
      MainBattleMenu ->
        case battleMenuIndex state of
          0 -> state {battleMenuType = FightMenu, battleMoveIndex = 0}
          2 -> state {battleMenuType = PokemonMenu, battleBenchIndex = firstSwitchableBenchIndex state}
          3 -> state {battleMenuType = QuitConfirmMenu, battleMoveIndex = 1}
          _ -> state
      FightMenu -> submitSelectedMove state
      BagMenu -> state
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
      QuitConfirmMenu ->
        case battleMoveIndex state of
          0 ->
            state
              { currentScreen = Menu,
                battleMenuType = MainBattleMenu,
                battleState = Nothing,
                battleMoveIndex = 0,
                battleMenuIndex = 0,
                selectedTrainer = Nothing,
                selectedTrainerIndex = 0,
                playerTeam = []
              }
          1 -> state {battleMenuType = MainBattleMenu}
          _ -> state

handleBattleBack :: GameState -> GameState
handleBattleBack state =
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

submitSelectedMove :: GameState -> GameState
submitSelectedMove state =
  case battleState state of
    Nothing -> state
    Just bState ->
      let action = ActionMove (battleMoveIndex state)
          (nextBattle, nextRng) = submitPlayerActionWithEnemyWeights (enemyAIWeights state) (randomGen state) bState action
       in state
            { battleState = Just nextBattle,
              randomGen = nextRng,
              currentScreen = battleResultScreenFrom nextBattle,
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
          (nextBattle, nextRng) = submitPlayerActionWithEnemyWeights (enemyAIWeights state) (randomGen state) bState action
       in state
            { battleState = Just nextBattle,
              randomGen = nextRng,
              currentScreen = battleResultScreenFrom nextBattle,
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
  [idx | (idx, bp) <- zip [0 ..] (playerBench bState), battlePokemonStatus bp /= Fainted]

nextSwitchableBenchIndex :: GameState -> Int -> Int
nextSwitchableBenchIndex state current =
  case battleState state of
    Nothing -> current
    Just bState ->
      let valid = switchableBenchIndices bState
       in case filter (> current) valid of
            (x : _) -> x
            [] -> case valid of
              [] -> 0
              _ -> head valid

previousSwitchableBenchIndex :: GameState -> Int -> Int
previousSwitchableBenchIndex state current =
  case battleState state of
    Nothing -> current
    Just bState ->
      let valid = switchableBenchIndices bState
       in case filter (< current) valid of
            [] -> case valid of
              [] -> 0
              _ -> last valid
            xs -> last xs
