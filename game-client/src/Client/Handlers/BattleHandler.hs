module Client.Handlers.BattleHandler
  ( handleUp,
    handleDown,
    handleLeft,
    handleRight,
    handleEnter,
    handleBack,
    isForcedSwitchPhase,
    isForcedEnemySwitchPhase,
    firstSwitchableBenchIndex,
    firstSwitchableBenchIndexFromBattle,
    nextSwitchableBenchIndex,
    previousSwitchableBenchIndex,
    nextBattleMenuType,
    battleResultScreenFrom,
  )
where

import Client.Types
  ( BattleMenuType (..),
    BattleScreenState (..),
    Screen (..),
    defaultBattleScreenState,
  )
import Pokemonad.AI.Model (QWeights)
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
  )
import Pokemonad.Battle.Turn (submitPlayerActionWithEnemyWeights)
import Pokemonad.Core.Types (Status (..))
import System.Random (StdGen)

-- ---------------------------------------------------------------------------
-- Directional handlers
-- ---------------------------------------------------------------------------

handleUp :: BattleScreenState -> BattleScreenState
handleUp s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
      _ -> s
    else case battleMenuType s of
      MainBattleMenu -> let c = battleMainCursor s in s {battleMainCursor = if c >= 2 then c - 2 else c}
      FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if c >= 2 then c - 2 else c}
      PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
      _ -> s

handleDown :: BattleScreenState -> BattleScreenState
handleDown s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
      _ -> s
    else case battleMenuType s of
      MainBattleMenu -> let c = battleMainCursor s in s {battleMainCursor = if c <= 1 then c + 2 else c}
      FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if c <= 1 then c + 2 else c}
      PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
      _ -> s

handleLeft :: BattleScreenState -> BattleScreenState
handleLeft s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 0}
      _ -> s
    else case battleMenuType s of
      MainBattleMenu -> let c = battleMainCursor s in s {battleMainCursor = if odd c then c - 1 else c}
      FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if odd c then c - 1 else c}
      PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 0}
      QuitConfirmMenu -> s {battleMoveCursor = 0}
      _ -> s

handleRight :: BattleScreenState -> BattleScreenState
handleRight s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 1}
      _ -> s
    else case battleMenuType s of
      MainBattleMenu -> let c = battleMainCursor s in s {battleMainCursor = if even c then c + 1 else c}
      FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if even c then c + 1 else c}
      PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 1}
      QuitConfirmMenu -> s {battleMoveCursor = 1}
      _ -> s

-- ---------------------------------------------------------------------------
-- Enter handler
-- ---------------------------------------------------------------------------

handleEnter ::
  Bool ->
  BattleScreenState ->
  Maybe QWeights ->
  StdGen ->
  (BattleScreenState, StdGen, Maybe Screen)
handleEnter isMP s weights gen
  | isForcedEnemySwitchPhase s = (s, gen, Nothing)
  | isForcedSwitchPhase s = case battleMenuType s of
      PokemonMenu ->
        case currentBattle s of
          Just bState ->
            if battleBenchCursor s < length (playerBench bState)
              then (s {battleMenuType = SwitchConfirmMenu, battleMoveCursor = 0}, gen, Nothing)
              else (s, gen, Nothing)
          Nothing -> (s, gen, Nothing)
      SwitchConfirmMenu ->
        case battleMoveCursor s of
          0 ->
            if isMP
              then storeLocalAction s gen (ActionSwitch (battleBenchCursor s))
              else submitSelectedSwitch s weights gen
          1 -> (s {battleMenuType = PokemonMenu}, gen, Nothing)
          _ -> (s, gen, Nothing)
      _ ->
        ( s
            { battleMenuType = PokemonMenu,
              battleMoveCursor = 0,
              battleBenchCursor = firstSwitchableBenchIndex s
            },
          gen,
          Nothing
        )
  | otherwise = case battleMenuType s of
      MainBattleMenu ->
        case battleMainCursor s of
          0 -> (s {battleMenuType = FightMenu, battleMoveCursor = 0}, gen, Nothing)
          2 -> (s {battleMenuType = PokemonMenu, battleBenchCursor = firstSwitchableBenchIndex s}, gen, Nothing)
          3 -> (s {battleMenuType = QuitConfirmMenu, battleMoveCursor = 1}, gen, Nothing)
          _ -> (s, gen, Nothing)
      FightMenu ->
        if isMP
          then storeLocalAction s gen (ActionMove (battleMoveCursor s))
          else submitSelectedMove s weights gen
      BagMenu -> (s, gen, Nothing)
      PokemonMenu ->
        case currentBattle s of
          Just bState ->
            if battleBenchCursor s < length (playerBench bState)
              then (s {battleMenuType = SwitchConfirmMenu, battleMoveCursor = 0}, gen, Nothing)
              else (s, gen, Nothing)
          Nothing -> (s, gen, Nothing)
      SwitchConfirmMenu ->
        case battleMoveCursor s of
          0 ->
            if isMP
              then storeLocalAction s gen (ActionSwitch (battleBenchCursor s))
              else submitSelectedSwitch s weights gen
          1 -> (s {battleMenuType = PokemonMenu}, gen, Nothing)
          _ -> (s, gen, Nothing)
      QuitConfirmMenu ->
        case battleMoveCursor s of
          0 -> (defaultBattleScreenState, gen, Just Menu)
          1 -> (s {battleMenuType = MainBattleMenu}, gen, Nothing)
          _ -> (s, gen, Nothing)

-- Store a local action and switch to waiting state (multiplayer).
storeLocalAction ::
  BattleScreenState ->
  StdGen ->
  BattleAction ->
  (BattleScreenState, StdGen, Maybe Screen)
storeLocalAction s gen action =
  ( s
      { battlePendingLocalAction = Just action,
        battleMenuType = MainBattleMenu,
        battleMoveCursor = 0
      },
    gen,
    Nothing
  )

-- ---------------------------------------------------------------------------
-- Back handler
-- ---------------------------------------------------------------------------

handleBack :: BattleScreenState -> BattleScreenState
handleBack s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      SwitchConfirmMenu -> s {battleMenuType = PokemonMenu, battleMoveCursor = 0}
      _ -> s
    else case battleMenuType s of
      FightMenu -> s {battleMenuType = MainBattleMenu}
      BagMenu -> s {battleMenuType = MainBattleMenu}
      PokemonMenu -> s {battleMenuType = MainBattleMenu}
      SwitchConfirmMenu -> s {battleMenuType = PokemonMenu, battleMoveCursor = 0}
      QuitConfirmMenu -> s {battleMenuType = MainBattleMenu}
      _ -> s

-- ---------------------------------------------------------------------------
-- Move / switch submission (single-player)
-- ---------------------------------------------------------------------------

submitSelectedMove ::
  BattleScreenState ->
  Maybe QWeights ->
  StdGen ->
  (BattleScreenState, StdGen, Maybe Screen)
submitSelectedMove s weights gen =
  case currentBattle s of
    Nothing -> (s, gen, Nothing)
    Just bState ->
      let action = ActionMove (battleMoveCursor s)
          (nextBattle, nextRng) = submitPlayerActionWithEnemyWeights weights gen bState action
          nextScreen = battleResultScreenFrom nextBattle
          nextMenuType = nextBattleMenuType nextBattle
          nextBenchCursor = firstSwitchableBenchIndexFromBattle nextBattle
          newState =
            s
              { currentBattle = Just nextBattle,
                battleMoveCursor = 0,
                battleBenchCursor = nextBenchCursor,
                battleMenuType = nextMenuType
              }
       in ( newState,
            nextRng,
            if nextScreen == BattleScreen then Nothing else Just nextScreen
          )

submitSelectedSwitch ::
  BattleScreenState ->
  Maybe QWeights ->
  StdGen ->
  (BattleScreenState, StdGen, Maybe Screen)
submitSelectedSwitch s weights gen =
  case currentBattle s of
    Nothing -> (s, gen, Nothing)
    Just bState ->
      let action = ActionSwitch (battleBenchCursor s)
          (nextBattle, nextRng) = submitPlayerActionWithEnemyWeights weights gen bState action
          nextScreen = battleResultScreenFrom nextBattle
          nextMenuType = nextBattleMenuType nextBattle
          nextBenchCursor = firstSwitchableBenchIndexFromBattle nextBattle
          newState =
            s
              { currentBattle = Just nextBattle,
                battleMoveCursor = 0,
                battleBenchCursor = nextBenchCursor,
                battleMenuType = nextMenuType
              }
       in ( newState,
            nextRng,
            if nextScreen == BattleScreen then Nothing else Just nextScreen
          )

-- ---------------------------------------------------------------------------
-- Predicates / helpers
-- ---------------------------------------------------------------------------

isForcedSwitchPhase :: BattleScreenState -> Bool
isForcedSwitchPhase s =
  case currentBattle s of
    Just bState -> phase bState == WaitingForForcedPlayerSwitch
    Nothing -> False

isForcedEnemySwitchPhase :: BattleScreenState -> Bool
isForcedEnemySwitchPhase s =
  case currentBattle s of
    Just bState -> phase bState == WaitingForForcedEnemySwitch
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

firstSwitchableBenchIndex :: BattleScreenState -> Int
firstSwitchableBenchIndex s = maybe 0 firstSwitchableBenchIndexFromBattle (currentBattle s)

firstSwitchableBenchIndexFromBattle :: BattleState -> Int
firstSwitchableBenchIndexFromBattle bState =
  case switchableBenchIndices bState of
    (x : _) -> x
    [] -> 0

switchableBenchIndices :: BattleState -> [Int]
switchableBenchIndices bState =
  [ idx
  | (idx, bp) <- zip [0 ..] (playerBench bState),
    battlePokemonStatus bp /= Fainted
  ]

nextSwitchableBenchIndex :: BattleScreenState -> Int -> Int
nextSwitchableBenchIndex s current =
  case currentBattle s of
    Nothing -> current
    Just bState ->
      let valid = switchableBenchIndices bState
       in case filter (> current) valid of
            (x : _) -> x
            [] -> case valid of
              [] -> 0
              (h : _) -> h

previousSwitchableBenchIndex :: BattleScreenState -> Int -> Int
previousSwitchableBenchIndex s current =
  case currentBattle s of
    Nothing -> current
    Just bState ->
      let valid = switchableBenchIndices bState
       in case filter (< current) valid of
            [] -> case valid of
              [] -> 0
              _ -> last valid
            xs -> last xs
