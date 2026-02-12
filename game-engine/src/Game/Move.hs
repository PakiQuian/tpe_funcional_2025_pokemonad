module Game.Move where

import Game.Pokemon (PokemonType (..))

data MoveCategory = Physical | Special | Status
  deriving (Show, Eq)

data Move = Move
  { mName :: String,
    mType :: PokemonType,
    mCategory :: MoveCategory,
    mPower :: Int, -- 0 si es Status
    mAccuracy :: Int, -- 0-100
    mPP :: Int, -- Power Points (cantidad de usos)
    mMaxPP :: Int
  }
  deriving (Show, Eq)

-- ==========================================
-- BASE DE DATOS DE MOVIMIENTOS
-- ==========================================

-- Helper para crear movimientos rápido
mkMove :: String -> PokemonType -> MoveCategory -> Int -> Int -> Int -> Move
mkMove name t cat pow acc pp = Move name t cat pow acc pp pp

-- Lista de Movimientos
allMoves :: [Move]
allMoves =
  [ -- NORMAL MOVES
    mkMove "Tackle" Normal Physical 40 100 35,
    mkMove "Scratch" Normal Physical 40 100 35,
    mkMove "Quick Attack" Normal Physical 40 100 30,
    mkMove "Hyper Beam" Normal Special 150 90 5,
    mkMove "Body Slam" Normal Physical 85 100 15,
    -- FIRE MOVES
    mkMove "Ember" Fire Special 40 100 25,
    mkMove "Flamethrower" Fire Special 90 100 15,
    mkMove "Fire Blast" Fire Special 110 85 5,
    -- WATER MOVES
    mkMove "Water Gun" Water Special 40 100 25,
    mkMove "Surf" Water Special 90 100 15,
    mkMove "Hydro Pump" Water Special 110 80 5,
    -- GRASS MOVES
    mkMove "Vine Whip" Grass Physical 45 100 25,
    mkMove "Razor Leaf" Grass Physical 55 95 25,
    mkMove "Solar Beam" Grass Special 120 100 10,
    -- ELECTRIC MOVES
    mkMove "Thundershock" Electric Special 40 100 30,
    mkMove "Thunderbolt" Electric Special 90 100 15,
    mkMove "Thunder" Electric Special 110 70 10,
    -- PSYCHIC MOVES
    mkMove "Confusion" Psychic Special 50 100 25,
    mkMove "Psychic" Psychic Special 90 100 10,
    -- GROUND/ROCK MOVES
    mkMove "Earthquake" Ground Physical 100 100 10,
    mkMove "Rock Slide" Rock Physical 75 90 10,
    -- FLYING
    mkMove "Wing Attack" Flying Physical 60 100 35,
    mkMove "Fly" Flying Physical 90 95 15,
    -- ICE
    mkMove "Ice Beam" Ice Special 90 100 10,
    mkMove "Blizzard" Ice Special 110 70 5,
    -- DRAGON
    mkMove "Dragon Claw" Dragon Physical 80 100 15,
    -- DEBUG MOVE (Para Paki)
    mkMove "Psystrike" Psychic Special 100 100 10 -- Mewtwo signature
  ]

-- Buscar movimiento por nombre
getMoveByName :: String -> Move
getMoveByName name =
  case filter (\m -> mName m == name) allMoves of
    (x : _) -> x
    [] -> head allMoves -- Fallback a Tackle si no existe