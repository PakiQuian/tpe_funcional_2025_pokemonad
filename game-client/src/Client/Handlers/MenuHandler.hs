module Client.Handlers.MenuHandler (handleUp, handleDown, handleEnter) where

import Client.Types (MenuState (..), Screen (..))

handleUp :: MenuState -> MenuState
handleUp s = s {menuCursor = max 0 (menuCursor s - 1)}

handleDown :: MenuState -> MenuState
handleDown s = s {menuCursor = min 3 (menuCursor s + 1)}

handleEnter :: MenuState -> (MenuState, Maybe Screen)
handleEnter s = (s, Just (chooseScreen (menuCursor s)))

chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = AISimulator
chooseScreen 3 = TeamSelect
chooseScreen _ = Menu
