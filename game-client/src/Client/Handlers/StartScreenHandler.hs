module Client.Handlers.StartScreenHandler (handleAnyKey) where

import Client.Types (Screen (..))

handleAnyKey :: Maybe Screen
handleAnyKey = Just Menu
