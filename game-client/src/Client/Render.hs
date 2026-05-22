module Client.Render (drawWorld) where

import Client.Screens.AISimulatorScreen (drawAISimulatorScreen)
import Client.Screens.BattleEndScreen (drawBattleEndScreen)
import qualified Client.Handlers.BattleHandler as BattleHandler
import Client.Screens.BattleScreen (drawBattleScreen)
import Client.Screens.MenuScreen (drawMenuScreen)
import Client.Screens.MultiplayerScreen (drawMultiplayerScreen)
import Client.Screens.OpponentSelectScreen (drawOpponentSelectScreen)
import Client.Screens.PokedexScreen (drawPokedexScreen)
import Client.Screens.PokemonScreen (drawPokemonScreen)
import Client.Screens.StartScreen (drawStartScreen)
import Client.Screens.TeamSelectScreen (drawTeamSelectScreen)
import Client.State (AppState (..), World (..))
import Client.Types
  ( AISimulatorState (..),
    Assets (..),
    BattleScreenState (..),
    MenuState (..),
    MultiplayerState (..),
    NetSubState (..),
    OpponentSelectState (..),
    PokedexState (..),
    Screen (..),
    TeamSelectState (..),
  )
import qualified Data.Map as Map
import Graphics.Gloss (Picture)
import Pokemonad.Battle.State (BattlePhase (..), BattleState (..), Winner (..))

drawWorld :: World -> IO Picture
drawWorld w = pure $ drawGame (worldGame w) (netSubState w)

drawGame :: AppState -> NetSubState -> Picture
drawGame gs netSt =
  let a = assets gs
      bss = battleScreenState gs
   in case currentScreen gs of
        StartScreen ->
          drawStartScreen (assetStartBg a)
        Menu ->
          drawMenuScreen (assetMenuBg a) (assetLogo a) (menuCursor (menuState gs))
        Pokedex ->
          let pid = pokedexCursor (pokedexState gs)
              maybeSprite = Map.lookup pid (assetPokeFront a)
           in drawPokedexScreen (assetMenuBg a) (assetLogo a) pid maybeSprite
        PokemonDetail ->
          let pid = pokedexCursor (pokedexState gs)
              maybeSprite = Map.lookup pid (assetPokeFront a)
           in drawPokemonScreen (assetMenuBg a) (assetLogo a) pid maybeSprite
        Multiplayer ->
          let inLobby = netSt == NetInLobby
           in drawMultiplayerScreen (assetMenuBg a) (assetLogo a) (multiplayerState gs) netSt inLobby
        AISimulator ->
          drawAISimulatorScreen (assetMenuBg a) (assetLogo a) (aiSimState gs)
        TeamSelect ->
          drawTeamSelectScreen
            (assetMenuBg a)
            (assetLogo a)
            (teamSelectCursor (teamSelectState gs))
            (playerTeam gs)
            (assetPokeFront a)
        OpponentSelect ->
          drawOpponentSelectScreen
            (assetMenuBg a)
            (assetLogo a)
            (trainerCursor (opponentState gs))
            (assetPokeFront a)
            (assetTrainers a)
        BattleScreen ->
          drawBattleScreen
            (assetBattleBgs a)
            (battleBgIndex bss)
            (currentBattle bss)
            (assetPokeFront a)
            (assetPokeBack a)
            (battleMainCursor bss)
            (battleMenuType bss)
            (battleMoveCursor bss)
            (battleBenchCursor bss)
            (battleShakeTarget bss)
            (battleShakeTimer bss)
            (BattleHandler.isWaitingForOpponent bss)
        BattleResultScreen ->
          case currentBattle bss of
            Just bState ->
              case phase bState of
                BattleEnded winner ->
                  let resultBg = case winner of
                        PlayerWon -> assetWinnerBg a
                        _ -> assetLoserBg a
                   in drawBattleEndScreen resultBg winner (currentBattle bss) (selectedTrainer gs) (assetTrainers a) (assetPokeFront a) (assetPokeBack a)
                _ ->
                  drawBattleEndScreen (assetWinnerBg a) PlayerWon (currentBattle bss) (selectedTrainer gs) (assetTrainers a) (assetPokeFront a) (assetPokeBack a)
            Nothing ->
              drawBattleEndScreen (assetWinnerBg a) PlayerWon (currentBattle bss) (selectedTrainer gs) (assetTrainers a) (assetPokeFront a) (assetPokeBack a)
