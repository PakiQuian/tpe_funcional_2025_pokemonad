module Game.Pokemon where

data PokemonType = Fire | Water | Grass | Normal | Electric | Bug | Flying
    deriving (Show, Eq)

data Stats = Stats
    { hp      :: Int
    , attack  :: Int
    , defense :: Int
    , speed   :: Int
    } deriving (Show, Eq)

data Pokemon = Pokemon
    { pId          :: Int
    , pName        :: String
    , pType        :: [PokemonType]
    , pStats       :: Stats
    , pDescription :: String
    , frontSprite  :: String 
    , backSprite   :: String
    } deriving (Show, Eq)

allPokemon :: [Pokemon]
allPokemon = 
    [ Pokemon 
        { pId = 1
        , pName = "BULBASAUR"
        , pType = [Grass, Normal]
        , pStats = Stats 45 49 49 45
        , pDescription = "A strange seed was planted on its back at birth."
        , frontSprite = "assets/pokemon/001_front.bmp"
        , backSprite  = "assets/pokemon/001_back.bmp"
        }
    , Pokemon 
        { pId = 4
        , pName = "CHARMANDER"
        , pType = [Fire]
        , pStats = Stats 39 52 43 65
        , pDescription = "Obviously prefers hot places."
        , frontSprite = "assets/pokemon/004_front.bmp"
        , backSprite  = "assets/pokemon/004_back.bmp"
        }
    ]

getPokemonById :: Int -> Maybe Pokemon
getPokemonById targetId = 
    if null matches then Nothing else Just (head matches)
  where 
    matches = filter (\p -> pId p == targetId) allPokemon