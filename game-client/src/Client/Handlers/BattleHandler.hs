module Client.Handlers.BattleHandler
  ( handleUp,
    handleDown,
    handleLeft,
    handleRight,
    handleEnter,
    handleBack,
    isAnimating,
    isWaitingForOpponent,
    isForcedSwitchPhase,
    isForcedEnemySwitchPhase,
    firstSwitchableBenchIndex,
    firstSwitchableBenchIndexFromBattle,
    nextSwitchableBenchIndex,
    previousSwitchableBenchIndex,
    nextBattleMenuType,
    battleResultScreenFrom,
    animationFrameSpacing,
    animationShakeDuration,
  )
where

import Client.Types
  ( BattleMenuType (..),
    BattleScreenState (..),
    Screen (..),
    defaultBattleScreenState,
  )
import Data.Maybe (isJust)
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

-- | True in multiplayer between the moment the user submits their action and
--   the moment the resulting frames arrive. Menu input is locked during this
--   window so the player can't accidentally re-submit a different action.
isWaitingForOpponent :: BattleScreenState -> Bool
isWaitingForOpponent s =
  battleIsMultiplayer s
    && isJust (battlePendingLocalAction s)
    && null (battlePendingFrames s)

-- | Seconds between frame pops in the per-step turn animation.
animationFrameSpacing :: Float
animationFrameSpacing = 0.6

-- | Seconds the defender sprite shakes after taking damage in a frame.
animationShakeDuration :: Float
animationShakeDuration = 0.25

-- ---------------------------------------------------------------------------
-- Directional handlers
-- ---------------------------------------------------------------------------

handleUp :: BattleScreenState -> BattleScreenState
handleUp s
  | isAnimating s = s
  | isWaitingForOpponent s = s
  | isForcedSwitchPhase s =
      case battleMenuType s of
        PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
        _ -> s
  | otherwise =
      case battleMenuType s of
        FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if c >= 2 then c - 2 else c}
        PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
        _ -> s

handleDown :: BattleScreenState -> BattleScreenState
handleDown s
  | isAnimating s = s
  | isWaitingForOpponent s = s
  | isForcedSwitchPhase s =
      case battleMenuType s of
        PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
        _ -> s
  | otherwise =
      case battleMenuType s of
        FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if c <= 1 then c + 2 else c}
        PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
        _ -> s

handleLeft :: BattleScreenState -> BattleScreenState
handleLeft s
  | isAnimating s = s
  | isWaitingForOpponent s = s
  | otherwise = handleLeft' s

handleLeft' :: BattleScreenState -> BattleScreenState
handleLeft' s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 0}
      _ -> s
    else case battleMenuType s of
      MainBattleMenu -> let c = battleMainCursor s in s {battleMainCursor = max 0 (c - 1)}
      FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if odd c then c - 1 else c}
      PokemonMenu -> s {battleBenchCursor = previousSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 0}
      QuitConfirmMenu -> s {battleMoveCursor = 0}

handleRight :: BattleScreenState -> BattleScreenState
handleRight s
  | isAnimating s = s
  | isWaitingForOpponent s = s
  | otherwise = handleRight' s

handleRight' :: BattleScreenState -> BattleScreenState
handleRight' s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 1}
      _ -> s
    else case battleMenuType s of
      MainBattleMenu -> let c = battleMainCursor s in s {battleMainCursor = min 2 (c + 1)}
      FightMenu -> let c = battleMoveCursor s in s {battleMoveCursor = if even c then c + 1 else c}
      PokemonMenu -> s {battleBenchCursor = nextSwitchableBenchIndex s (battleBenchCursor s)}
      SwitchConfirmMenu -> s {battleMoveCursor = 1}
      QuitConfirmMenu -> s {battleMoveCursor = 1}

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
  | isAnimating s = (s, gen, Nothing)
  | isWaitingForOpponent s = (s, gen, Nothing)
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
          1 -> (s {battleMenuType = PokemonMenu, battleBenchCursor = firstSwitchableBenchIndex s}, gen, Nothing)
          2 -> (s {battleMenuType = QuitConfirmMenu, battleMoveCursor = 1}, gen, Nothing)
          _ -> (s, gen, Nothing)
      FightMenu ->
        if isMP
          then storeLocalAction s gen (ActionMove (battleMoveCursor s))
          else submitSelectedMove s weights gen
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
handleBack s
  | isAnimating s = s
  | isWaitingForOpponent s = s
  | otherwise = handleBack' s

handleBack' :: BattleScreenState -> BattleScreenState
handleBack' s =
  if isForcedSwitchPhase s
    then case battleMenuType s of
      SwitchConfirmMenu -> s {battleMenuType = PokemonMenu, battleMoveCursor = 0}
      _ -> s
    else case battleMenuType s of
      FightMenu -> s {battleMenuType = MainBattleMenu}
      PokemonMenu -> s {battleMenuType = MainBattleMenu}
      SwitchConfirmMenu -> s {battleMenuType = PokemonMenu, battleMoveCursor = 0}
      QuitConfirmMenu -> s {battleMenuType = MainBattleMenu}
      _ -> s

-- ---------------------------------------------------------------------------
-- Move / switch submission (single-player)
-- ---------------------------------------------------------------------------

-- | True while the battle is in the middle of replaying turn frames; menu
--   input is ignored during this window.
isAnimating :: BattleScreenState -> Bool
isAnimating s = not (null (battlePendingFrames s))

-- | Queue a player's action for animated replay. The pre-turn state stays in
--   `currentBattle` until the first frame is popped by the tick handler.
queuePlayerAction ::
  BattleScreenState ->
  Maybe QWeights ->
  StdGen ->
  BattleAction ->
  (BattleScreenState, StdGen, Maybe Screen)
queuePlayerAction s weights gen action =
  case currentBattle s of
    Nothing -> (s, gen, Nothing)
    Just bState ->
      let (frames, nextRng) = submitPlayerActionWithEnemyWeights weights gen bState action
          newState =
            s
              { battlePendingFrames = frames,
                battleFrameTimer = animationFrameSpacing,
                battleMoveCursor = 0
              }
       in (newState, nextRng, Nothing)

submitSelectedMove ::
  BattleScreenState ->
  Maybe QWeights ->
  StdGen ->
  (BattleScreenState, StdGen, Maybe Screen)
submitSelectedMove s weights gen = queuePlayerAction s weights gen (ActionMove (battleMoveCursor s))

submitSelectedSwitch ::
  BattleScreenState ->
  Maybe QWeights ->
  StdGen ->
  (BattleScreenState, StdGen, Maybe Screen)
submitSelectedSwitch s weights gen = queuePlayerAction s weights gen (ActionSwitch (battleBenchCursor s))

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
