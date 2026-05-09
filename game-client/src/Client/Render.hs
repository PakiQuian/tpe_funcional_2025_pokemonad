module Client.Render (drawWorld) where

import qualified Data.Map as Map
import Client.State (GameState (..), World (..))
import Client.Types (NetSubState, Screen (..))
import Client.Screens.AISimulatorScreen (drawAISimulatorScreen)
import Client.Screens.BattleEndScreen (drawBattleEndScreen)
import Client.Screens.BattleScreen (drawBattleScreen)
import Client.Screens.MenuScreen (drawMenuScreen)
import Client.Screens.MultiplayerScreen (drawMultiplayerScreen)
import Client.Screens.OpponentSelectScreen (drawOpponentSelectScreen)
import Client.Screens.PokedexScreen (drawPokedexScreen)
import Client.Screens.PokemonScreen (drawPokemonScreen)
import Client.Screens.StartScreen (drawStartScreen)
import Client.Screens.TeamSelectScreen (drawTeamSelectScreen)
import Graphics.Gloss (Picture)
import Pokemonad.Battle.State (BattlePhase (..), BattleState (..), Winner (..))

drawWorld :: World -> IO Picture
drawWorld w = pure $ drawGame (worldGame w) (netSubState w)

drawGame :: GameState -> NetSubState -> Picture
drawGame gs netSt = case currentScreen gs of
  StartScreen -> drawStartScreen (startBgImage gs)
  Menu -> drawMenuScreen (menuBgImage gs) (logoImage gs) (selectedOption gs)
  Pokedex ->
    let maybeSprite = Map.lookup (selectedPokemonId gs) (pokemonFrontSprites gs)
     in drawPokedexScreen (menuBgImage gs) (logoImage gs) (selectedPokemonId gs) maybeSprite
  PokemonDetail ->
    let maybeSprite = Map.lookup (selectedPokemonId gs) (pokemonFrontSprites gs)
     in drawPokemonScreen (menuBgImage gs) (logoImage gs) (selectedPokemonId gs) maybeSprite
  Multiplayer -> drawMultiplayerScreen (menuBgImage gs) (logoImage gs) gs netSt
  AISimulator -> drawAISimulatorScreen (menuBgImage gs) (logoImage gs) gs
  TeamSelect ->
    drawTeamSelectScreen
      (menuBgImage gs)
      (logoImage gs)
      (selectedPokemonId gs)
      (playerTeam gs)
      (pokemonFrontSprites gs)
  OpponentSelect ->
    drawOpponentSelectScreen
      (menuBgImage gs)
      (logoImage gs)
      (selectedTrainerIndex gs)
      (pokemonFrontSprites gs)
      (trainerSprites gs)
  BattleScreen ->
    drawBattleScreen
      (battleBackgrounds gs)
      (currentBattleBg gs)
      (battleState gs)
      (pokemonFrontSprites gs)
      (pokemonBackSprites gs)
      (battleMenuIndex gs)
      (battleMenuType gs)
      (battleMoveIndex gs)
      (battleBenchIndex gs)
  BattleResultScreen ->
    case battleState gs of
      Just bState ->
        case phase bState of
          BattleEnded winner ->
            let resultBg = case winner of
                  PlayerWon -> winnerBgImage gs
                  _ -> loserBgImage gs
             in drawBattleEndScreen resultBg winner (battleState gs) (selectedTrainer gs) (trainerSprites gs) (pokemonFrontSprites gs) (pokemonBackSprites gs)
          _ ->
            drawBattleEndScreen (winnerBgImage gs) PlayerWon (battleState gs) (selectedTrainer gs) (trainerSprites gs) (pokemonFrontSprites gs) (pokemonBackSprites gs)
      Nothing ->
        drawBattleEndScreen (winnerBgImage gs) PlayerWon (battleState gs) (selectedTrainer gs) (trainerSprites gs) (pokemonFrontSprites gs) (pokemonBackSprites gs)
