module Client.Events
  ( handleWorldInput,
    handleWorldTick,
  )
where

import qualified Client.Handlers.AISimulatorHandler as AISimulatorHandler
import qualified Client.Handlers.BattleHandler as BattleHandler
import qualified Client.Handlers.BattleResultHandler as BattleResultHandler
import qualified Client.Handlers.MenuHandler as MenuHandler
import qualified Client.Handlers.MultiplayerHandler as MultiplayerHandler
import qualified Client.Handlers.OpponentSelectHandler as OpponentSelectHandler
import qualified Client.Handlers.PokedexHandler as PokedexHandler
import qualified Client.Handlers.PokemonDetailHandler as PokemonDetailHandler
import qualified Client.Handlers.StartScreenHandler as StartScreenHandler
import qualified Client.Handlers.TeamSelectHandler as TeamSelectHandler
import Client.State
  ( AppState (..),
    World (..),
    disconnectNetWorld,
    drainNetInbox,
    mergeMultiplayerBattle,
    mergeMultiplayerLobby,
    mergeNetAsync,
  )
import Client.Types
  ( Assets (..),
    BattleMenuType (..),
    BattleScreenState (..),
    MultiplayerState (..),
    NetSubState (..),
    OpponentSelectState (..),
    Screen (..),
    defaultBattleScreenState,
  )
import Data.Maybe (isJust)
import Data.Word (Word32)
import Graphics.Gloss.Interface.Pure.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down, Up),
    SpecialKey (KeyBackspace, KeyDelete, KeyDown, KeyEnter, KeyEsc, KeyLeft, KeyRight, KeyUp),
  )
import P2P.Communication (sendMsg)
import P2P.Types (AppMsg (..), PlayerAction (..))
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
    Side (..),
  )
import Pokemonad.Core.Types (HP (..), PokemonId (..))

-- ---------------------------------------------------------------------------
-- World-level IO entry points
-- ---------------------------------------------------------------------------

handleWorldInput :: Event -> World -> IO World
handleWorldInput ev w = do
  let g0 = worldGame w
  w1 <- AISimulatorHandler.launchAITrainingIfRequested ev w
  let g1 = handleGameInput ev (worldGame w1)
      leftMP =
        netSubState w /= NetDisconnected
          && currentScreen g1 == Menu
          && currentScreen g0 `elem` [Multiplayer, TeamSelect, OpponentSelect, BattleScreen, BattleResultScreen]
  w2 <- if leftMP then disconnectNetWorld w1 else pure w1

  -- Intercept TeamSelect → OpponentSelect when in multiplayer lobby
  let teamJustConfirmed =
        currentScreen g0 == TeamSelect
          && currentScreen g1 == OpponentSelect
          && netSubState w == NetInLobby
  w3 <-
    if teamJustConfirmed
      then sendTeamAndWait (playerTeam g1) (w2 {worldGame = g1})
      else pure (w2 {worldGame = g1})

  -- Send pending local action to host when client just committed an action
  let bss0 = battleScreenState g0
      bss3 = battleScreenState (worldGame w3)
      actionJustSet =
        battlePendingLocalAction bss3 /= battlePendingLocalAction bss0
          && isJust (battlePendingLocalAction bss3)
  w4 <-
    if actionJustSet && not (netIsHost w3) && netSubState w3 == NetInBattle
      then sendPendingAction w3
      else pure w3

  -- Process pending connection intent
  let g4 = worldGame w4
      mp = multiplayerState g4
  case mpPending mp of
    Nothing -> pure w4
    Just intent ->
      let g5 = g4 {multiplayerState = mp {mpPending = Nothing}}
       in MultiplayerHandler.startMultiplayerNet intent (w4 {worldGame = g5})

handleWorldTick :: Float -> World -> IO World
handleWorldTick dt w = do
  wMerged <- mergeNetAsync w
  wDrained <- drainNetInbox wMerged
  wLobby <- mergeMultiplayerLobby wDrained
  wBattle <- mergeMultiplayerBattle wLobby
  wAI <- AISimulatorHandler.mergeAITraining wBattle
  pure wAI {worldGame = handleTick dt (worldGame wAI)}

-- ---------------------------------------------------------------------------
-- Multiplayer IO helpers
-- ---------------------------------------------------------------------------

sendTeamAndWait :: [PokemonId] -> World -> IO World
sendTeamAndWait teamIds w = do
  let ids = map (fromIntegral . unPokemonId) teamIds :: [Word32]
      ms = multiplayerState (worldGame w)
      newMs = ms {mpTeamSent = True}
      newGs = (worldGame w) {currentScreen = Multiplayer, multiplayerState = newMs}
  case netSocket w of
    Just sock -> sendMsg sock (AppMsgTeam ids)
    Nothing -> pure ()
  pure w {worldGame = newGs}

sendPendingAction :: World -> IO World
sendPendingAction w = do
  let bss = battleScreenState (worldGame w)
  case (netSocket w, battlePendingLocalAction bss) of
    (Just sock, Just action) -> sendMsg sock (AppMsgAction (toPlayerAction action))
    _ -> pure ()
  pure w

toPlayerAction :: BattleAction -> PlayerAction
toPlayerAction (ActionMove idx) = UseMove idx
toPlayerAction (ActionSwitch idx) = SwitchPokemon idx

-- ---------------------------------------------------------------------------
-- Pure game input handler
-- ---------------------------------------------------------------------------

handleGameInput :: Event -> AppState -> AppState
handleGameInput ev gs = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> applyScrollStart True gs
  EventKey (SpecialKey KeyUp) Up _ _ -> gs {holdingUp = False}
  EventKey (SpecialKey KeyDown) Down _ _ -> applyScrollStart False gs
  EventKey (SpecialKey KeyDown) Up _ _ -> gs {holdingDown = False}
  EventKey (SpecialKey KeyLeft) Down _ _ -> dispatchLeft gs
  EventKey (SpecialKey KeyRight) Down _ _ -> dispatchRight gs
  EventKey (SpecialKey KeyEnter) Down _ _ -> dispatchEnter gs
  EventKey (SpecialKey KeyBackspace) Down _ _ -> dispatchBack gs
  EventKey (SpecialKey KeyDelete) Down _ _ -> dispatchBack gs
  EventKey (Char '\b') Down _ _ -> dispatchBack gs
  EventKey (SpecialKey KeyEsc) Down _ _ -> dispatchBack gs
  EventKey (Char 'r') Down _ _ -> dispatchCharR gs
  EventKey (Char 'R') Down _ _ -> dispatchCharR gs
  EventKey (Char c) Down _ _ -> dispatchChar c gs
  EventKey _ Down _ _ -> dispatchAnyKey gs
  _ -> gs

handleTick :: Float -> AppState -> AppState
handleTick dt gs =
  let bs = battleScreenState gs
      onBattle = currentScreen gs == BattleScreen
   in if onBattle && BattleHandler.isAnimating bs
        then advanceBattleFrames dt gs
        else advanceShake dt (handleScroll dt gs)

-- | While a turn animation is in flight, advance the frame timer and pop the
--   next frame whenever the spacing threshold is crossed. Other input is
--   gated off (see BattleHandler.handle*).
advanceBattleFrames :: Float -> AppState -> AppState
advanceBattleFrames dt gs =
  let bs = battleScreenState gs
      newTimer = battleFrameTimer bs + dt
      threshold = BattleHandler.animationFrameSpacing
   in if newTimer < threshold
        then gs {battleScreenState = decayShake dt (bs {battleFrameTimer = newTimer})}
        else popFrame (newTimer - threshold) gs

-- | Apply the next frame in the queue: update currentBattle, set shake target
--   for any side that lost HP, and \(when the queue empties\) restore the
--   appropriate menu type / transition to the result screen.
popFrame :: Float -> AppState -> AppState
popFrame leftover gs =
  let bs = battleScreenState gs
   in case battlePendingFrames bs of
        [] -> gs
        ((frameState, _frameLogs) : rest) ->
          let prev = currentBattle bs
              shakeSide = damagedSide prev frameState
              queueEmptyAfter = null rest
              endingMenuType
                | not queueEmptyAfter = battleMenuType bs
                | phase frameState == WaitingForForcedPlayerSwitch = PokemonMenu
                | otherwise = MainBattleMenu
              endingBenchCursor
                | queueEmptyAfter && phase frameState == WaitingForForcedPlayerSwitch =
                    BattleHandler.firstSwitchableBenchIndexFromBattle frameState
                | otherwise = battleBenchCursor bs
              endingScreen
                | not queueEmptyAfter = currentScreen gs
                | otherwise = case phase frameState of
                    BattleEnded _ -> BattleResultScreen
                    _ -> BattleScreen
              bs' =
                bs
                  { currentBattle = Just frameState,
                    battlePendingFrames = rest,
                    battleFrameTimer = leftover,
                    battleShakeTimer = maybe 0 (const BattleHandler.animationShakeDuration) shakeSide,
                    battleShakeTarget = shakeSide,
                    battleMenuType = endingMenuType,
                    battleBenchCursor = endingBenchCursor,
                    battleMoveCursor = if queueEmptyAfter then 0 else battleMoveCursor bs
                  }
           in gs {battleScreenState = bs', currentScreen = endingScreen}

-- | Detect which side took damage between two consecutive frames, if any.
damagedSide :: Maybe BattleState -> BattleState -> Maybe Side
damagedSide Nothing _ = Nothing
damagedSide (Just prev) next
  | hp playerActive next < hp playerActive prev = Just PlayerSide
  | hp enemyActive next < hp enemyActive prev = Just EnemySide
  | otherwise = Nothing
  where
    hp accessor st = unHP (battlePokemonHp (accessor st))

-- | Decay the shake timer; clear the target when it elapses.
advanceShake :: Float -> AppState -> AppState
advanceShake dt gs = gs {battleScreenState = decayShake dt (battleScreenState gs)}

decayShake :: Float -> BattleScreenState -> BattleScreenState
decayShake dt bs =
  let nextTimer = max 0 (battleShakeTimer bs - dt)
   in bs
        { battleShakeTimer = nextTimer,
          battleShakeTarget = if nextTimer <= 0 then Nothing else battleShakeTarget bs
        }

-- | Continuous up/down scroll handling (menu navigation).
handleScroll :: Float -> AppState -> AppState
handleScroll dt gs =
  let scrollSpeed = 0.05
      newTimer = scrollTimer gs + dt
   in if holdingUp gs && newTimer >= scrollSpeed
        then dispatchUp (gs {scrollTimer = 0})
        else
          if holdingDown gs && newTimer >= scrollSpeed
            then dispatchDown (gs {scrollTimer = 0})
            else gs {scrollTimer = newTimer}

-- ---------------------------------------------------------------------------
-- Scroll helpers
-- ---------------------------------------------------------------------------

applyScrollStart :: Bool -> AppState -> AppState
applyScrollStart isUp gs =
  let gs' =
        if isUp
          then gs {holdingUp = True, scrollTimer = -0.3}
          else gs {holdingDown = True, scrollTimer = -0.3}
   in if isUp then dispatchUp gs' else dispatchDown gs'

-- ---------------------------------------------------------------------------
-- Directional dispatch
-- ---------------------------------------------------------------------------

dispatchUp :: AppState -> AppState
dispatchUp gs = case currentScreen gs of
  Menu -> gs {menuState = MenuHandler.handleUp (menuState gs)}
  Multiplayer -> gs {multiplayerState = (multiplayerState gs) {mpCursor = max 0 (mpCursor (multiplayerState gs) - 1)}}
  Pokedex -> gs {pokedexState = PokedexHandler.handleUp (pokedexState gs)}
  TeamSelect -> gs {teamSelectState = TeamSelectHandler.handleUp (teamSelectState gs)}
  OpponentSelect -> gs {opponentState = OpponentSelectHandler.handleUp (opponentState gs)}
  BattleScreen -> gs {battleScreenState = BattleHandler.handleUp (battleScreenState gs)}
  _ -> gs

dispatchDown :: AppState -> AppState
dispatchDown gs = case currentScreen gs of
  Menu -> gs {menuState = MenuHandler.handleDown (menuState gs)}
  Multiplayer -> gs {multiplayerState = (multiplayerState gs) {mpCursor = min 4 (mpCursor (multiplayerState gs) + 1)}}
  Pokedex -> gs {pokedexState = PokedexHandler.handleDown (pokedexState gs)}
  TeamSelect -> gs {teamSelectState = TeamSelectHandler.handleDown (teamSelectState gs)}
  OpponentSelect -> gs {opponentState = OpponentSelectHandler.handleDown (opponentState gs)}
  BattleScreen -> gs {battleScreenState = BattleHandler.handleDown (battleScreenState gs)}
  _ -> gs

dispatchLeft :: AppState -> AppState
dispatchLeft gs = case currentScreen gs of
  BattleScreen -> gs {battleScreenState = BattleHandler.handleLeft (battleScreenState gs)}
  _ -> gs

dispatchRight :: AppState -> AppState
dispatchRight gs = case currentScreen gs of
  BattleScreen -> gs {battleScreenState = BattleHandler.handleRight (battleScreenState gs)}
  _ -> gs

-- ---------------------------------------------------------------------------
-- Enter dispatch
-- ---------------------------------------------------------------------------

dispatchEnter :: AppState -> AppState
dispatchEnter gs = case currentScreen gs of
  StartScreen ->
    applyTransition StartScreenHandler.handleAnyKey gs
  Menu ->
    let (ms', trans) = MenuHandler.handleEnter (menuState gs)
     in applyTransition trans (gs {menuState = ms'})
  Pokedex ->
    applyTransition (PokedexHandler.handleEnter (pokedexState gs)) gs
  PokemonDetail -> gs
  TeamSelect ->
    let (ts', team', trans) = TeamSelectHandler.handleEnter (teamSelectState gs) (playerTeam gs)
     in applyTransition trans (gs {teamSelectState = ts', playerTeam = team'})
  OpponentSelect ->
    let bgCount = length (assetBattleBgs (assets gs))
        (os', trainer, bss', rng', trans) =
          OpponentSelectHandler.handleEnter (opponentState gs) (playerTeam gs) bgCount (randomGen gs)
     in applyTransition
          trans
          (gs {opponentState = os', selectedTrainer = Just trainer, battleScreenState = bss', randomGen = rng'})
  Multiplayer ->
    let (ms', trans) = MultiplayerHandler.handleEnter (multiplayerState gs)
     in applyTransition trans (gs {multiplayerState = ms'})
  AISimulator -> gs
  BattleScreen ->
    let bss = battleScreenState gs
        isMP = battleIsMultiplayer bss
        (bss', rng', trans) =
          BattleHandler.handleEnter isMP bss (enemyAIWeights gs) (randomGen gs)
     in applyTransition trans (gs {battleScreenState = bss', randomGen = rng'})
  BattleResultScreen ->
    let (trainer', team', trans) = BattleResultHandler.handleEnter
     in applyTransition trans (gs {selectedTrainer = trainer', playerTeam = team'})

-- ---------------------------------------------------------------------------
-- Back dispatch
-- ---------------------------------------------------------------------------

dispatchBack :: AppState -> AppState
dispatchBack gs = case currentScreen gs of
  StartScreen -> gs
  Menu -> gs
  Multiplayer ->
    let (ms', trans) = MultiplayerHandler.handleBack (multiplayerState gs)
     in applyTransition trans (gs {multiplayerState = ms'})
  TeamSelect ->
    let (ts', team', trans) = TeamSelectHandler.handleBack (teamSelectState gs) (playerTeam gs)
     in applyTransition trans (gs {teamSelectState = ts', playerTeam = team'})
  BattleScreen ->
    gs {battleScreenState = BattleHandler.handleBack (battleScreenState gs)}
  Pokedex ->
    applyTransition (PokedexHandler.handleBack (pokedexState gs)) gs
  PokemonDetail ->
    applyTransition PokemonDetailHandler.handleBack gs
  AISimulator ->
    applyTransition (AISimulatorHandler.handleBack (aiSimState gs)) gs
  _ -> applyTransition (Just Menu) gs

-- ---------------------------------------------------------------------------
-- Char / charR / anyKey dispatch
-- ---------------------------------------------------------------------------

dispatchCharR :: AppState -> AppState
dispatchCharR gs = case currentScreen gs of
  TeamSelect ->
    let (ts', rng', team') = TeamSelectHandler.handleRandomTeam (teamSelectState gs) (randomGen gs)
     in gs {teamSelectState = ts', randomGen = rng', playerTeam = team'}
  _ -> gs

dispatchChar :: Char -> AppState -> AppState
dispatchChar c gs = case currentScreen gs of
  Multiplayer -> gs {multiplayerState = MultiplayerHandler.handleChar c (multiplayerState gs)}
  _ -> gs

dispatchAnyKey :: AppState -> AppState
dispatchAnyKey gs = case currentScreen gs of
  StartScreen -> applyTransition StartScreenHandler.handleAnyKey gs
  _ -> gs

-- ---------------------------------------------------------------------------
-- Screen transition helpers
-- ---------------------------------------------------------------------------

applyTransition :: Maybe Screen -> AppState -> AppState
applyTransition Nothing gs = gs
applyTransition (Just newScreen) gs =
  let cleaned = applyTransitionCleanup (currentScreen gs) newScreen gs
   in cleaned {currentScreen = newScreen}

applyTransitionCleanup :: Screen -> Screen -> AppState -> AppState
applyTransitionCleanup fromScreen Menu gs = case fromScreen of
  BattleScreen -> resetBattleGlobals gs
  BattleResultScreen -> resetBattleGlobals gs
  _ -> gs
applyTransitionCleanup _ _ gs = gs

resetBattleGlobals :: AppState -> AppState
resetBattleGlobals gs =
  gs
    { selectedTrainer = Nothing,
      playerTeam = [],
      opponentState = (opponentState gs) {trainerCursor = 0},
      battleScreenState = defaultBattleScreenState
    }
