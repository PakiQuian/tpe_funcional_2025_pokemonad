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
    [ -- BULBASAUR FAMILY
      Pokemon 
        { pId = 1, pName = "BULBASAUR", pType = [Grass], pStats = Stats 45 49 49 45
        , pDescription = "A strange seed was planted on its back at birth."
        , frontSprite = "game-client/assets/pokemon/001_front.png"
        , backSprite  = "game-client/assets/pokemon/001_back.png" }
    , Pokemon 
        { pId = 2, pName = "IVYSAUR", pType = [Grass], pStats = Stats 60 62 63 60
        , pDescription = "Exposure to sunlight adds to its strength."
        , frontSprite = "game-client/assets/pokemon/002_front.png"
        , backSprite  = "game-client/assets/pokemon/002_back.png" }
    , Pokemon 
        { pId = 3, pName = "VENUSAUR", pType = [Grass], pStats = Stats 80 82 83 80
        , pDescription = "The flower on its back catches the sun's rays."
        , frontSprite = "game-client/assets/pokemon/003_front.png"
        , backSprite  = "game-client/assets/pokemon/003_back.png" }
    
    -- CHARMANDER FAMILY
    , Pokemon 
        { pId = 4, pName = "CHARMANDER", pType = [Fire], pStats = Stats 39 52 43 65
        , pDescription = "Obviously prefers hot places."
        , frontSprite = "game-client/assets/pokemon/004_front.png"
        , backSprite  = "game-client/assets/pokemon/004_back.png" }
    , Pokemon 
        { pId = 5, pName = "CHARMELEON", pType = [Fire], pStats = Stats 58 64 58 80
        , pDescription = "When it swings its burning tail, it elevates the temperature."
        , frontSprite = "game-client/assets/pokemon/005_front.png"
        , backSprite  = "game-client/assets/pokemon/005_back.png" }
    , Pokemon 
        { pId = 6, pName = "CHARIZARD", pType = [Fire, Flying], pStats = Stats 78 84 78 100
        , pDescription = "Spits fire that is hot enough to melt boulders."
        , frontSprite = "game-client/assets/pokemon/006_front.png"
        , backSprite  = "game-client/assets/pokemon/006_back.png" }

    -- SQUIRTLE FAMILY
    , Pokemon 
        { pId = 7, pName = "SQUIRTLE", pType = [Water], pStats = Stats 44 48 65 43
        , pDescription = "Shoots water at prey while it is in the water."
        , frontSprite = "game-client/assets/pokemon/007_front.png"
        , backSprite  = "game-client/assets/pokemon/007_back.png" }
    , Pokemon 
        { pId = 8, pName = "WARTORTLE", pType = [Water], pStats = Stats 59 63 80 58
        , pDescription = "Often hides in water to stalk unwary prey."
        , frontSprite = "game-client/assets/pokemon/008_front.png"
        , backSprite  = "game-client/assets/pokemon/008_back.png" }
    , Pokemon 
        { pId = 9, pName = "BLASTOISE", pType = [Water], pStats = Stats 79 83 100 78
        , pDescription = "The jets of water it spouts from the rocket cannons."
        , frontSprite = "game-client/assets/pokemon/009_front.png"
        , backSprite  = "game-client/assets/pokemon/009_back.png" }
    ]

getPokemonById :: Int -> Maybe Pokemon
getPokemonById targetId = 
    if null matches then Nothing else Just (head matches)
  where 
    matches = filter (\p -> pId p == targetId) allPokemon