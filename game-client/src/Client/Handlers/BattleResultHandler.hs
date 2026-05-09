module Client.Handlers.BattleResultHandler (handleEnter) where

import Client.Types (Screen (..))
import Pokemonad.Core.Trainer (Trainer)
import Pokemonad.Core.Types (PokemonId)

handleEnter :: (Maybe Trainer, [PokemonId], Maybe Screen)
handleEnter = (Nothing, [], Just Menu)
