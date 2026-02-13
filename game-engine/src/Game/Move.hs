module Game.Move where

import Game.Types (PokemonType (..))

data MoveCategory = Physical | Special | Status
  deriving (Show, Eq)

data Move = Move
  { mName :: String,
    mType :: PokemonType,
    mCategory :: MoveCategory,
    mPower :: Int,
    mAccuracy :: Int,
    mPP :: Int,
    mMaxPP :: Int
  }
  deriving (Show, Eq)

mkMove :: String -> PokemonType -> MoveCategory -> Int -> Int -> Int -> Move
mkMove name t cat pow acc pp = Move name t cat pow acc pp pp

-- ==========================================
-- BASE DE DATOS DE MOVIMIENTOS
-- ==========================================

allMoves :: [Move]
allMoves =
  [ -- NORMAL MOVES (PHYSICAL)
    mkMove "Tackle" Normal Physical 40 100 35,
    mkMove "Scratch" Normal Physical 40 100 35,
    mkMove "Quick Attack" Normal Physical 40 100 30,
    mkMove "Body Slam" Normal Physical 85 100 15,
    mkMove "Double-edge" Normal Physical 120 100 15,
    mkMove "Take Down" Normal Physical 90 85 20,
    mkMove "Slash" Normal Physical 70 100 20,
    mkMove "Mega Punch" Normal Physical 80 85 20,
    mkMove "Mega Kick" Normal Physical 120 75 5,
    mkMove "Slam" Normal Physical 80 75 20,
    mkMove "Horn Drill" Normal Physical 1 30 5, -- OHKO (Fixed dmg logic handled by engine)
    mkMove "Guillotine" Normal Physical 1 30 5, -- OHKO
    mkMove "Fury Attack" Normal Physical 15 85 20,
    mkMove "Fury Swipes" Normal Physical 18 80 15,
    mkMove "Wrap" Normal Physical 15 90 20,
    mkMove "Bind" Normal Physical 15 85 20,
    mkMove "Stomp" Normal Physical 65 100 20,
    mkMove "Headbutt" Normal Physical 70 100 15,
    mkMove "Spike Cannon" Normal Physical 20 100 15,
    mkMove "Egg Bomb" Normal Physical 100 75 10,
    mkMove "Constrict" Normal Physical 10 100 35,
    mkMove "Skull Bash" Normal Physical 130 100 10,
    mkMove "Strength" Normal Physical 80 100 15,
    mkMove "Thrash" Normal Physical 120 100 10,
    mkMove "Cut" Normal Physical 50 95 30,
    mkMove "Explosion" Normal Physical 250 100 5,
    mkMove "Selfdestruct" Normal Physical 200 100 5,
    mkMove "Struggle" Normal Physical 50 100 1,
    mkMove "Bide" Normal Physical 1 100 10, -- Variable damage
    mkMove "Rage" Normal Physical 20 100 20,
    mkMove "Hyper Fang" Normal Physical 80 90 15,
    mkMove "Dizzy Punch" Normal Physical 70 100 10,
    -- NORMAL MOVES (SPECIAL)
    mkMove "Hyper Beam" Normal Special 150 90 5,
    mkMove "Swift" Normal Special 60 100 20, -- Never miss logic handled by engine
    mkMove "Tri Attack" Normal Special 80 100 10,
    mkMove "Razor Wind" Normal Special 80 100 10,
    -- NORMAL MOVES (STATUS)
    mkMove "Growth" Normal Status 0 100 40,
    mkMove "Swords Dance" Normal Status 0 100 30,
    mkMove "Double Team" Normal Status 0 100 15,
    mkMove "Recover" Normal Status 0 100 10,
    mkMove "Minimize" Normal Status 0 100 10,
    mkMove "Screech" Normal Status 0 85 40,
    mkMove "Sing" Normal Status 0 55 15,
    mkMove "Growl" Normal Status 0 100 40,
    mkMove "Roar" Normal Status 0 100 20,
    mkMove "Disable" Normal Status 0 100 20,
    mkMove "Supersonic" Normal Status 0 55 20,
    mkMove "Whirlwind" Normal Status 0 100 20,
    mkMove "Leer" Normal Status 0 100 30,
    mkMove "Tail Whip" Normal Status 0 100 30,
    mkMove "Glare" Normal Status 0 100 30,
    mkMove "Lovely Kiss" Normal Status 0 75 10,
    mkMove "Transform" Normal Status 0 100 10,
    mkMove "Substitute" Normal Status 0 100 10,
    mkMove "Metronome" Normal Status 0 100 10,
    mkMove "Mimic" Normal Status 0 100 10,
    mkMove "Splash" Normal Status 0 100 40,
    mkMove "Harden" Normal Status 0 100 30,
    mkMove "Sharpen" Normal Status 0 100 30,
    mkMove "Conversion" Normal Status 0 100 30,
    mkMove "Focus Energy" Normal Status 0 100 30,
    mkMove "Flash" Normal Status 0 100 20,
    -- FIRE MOVES
    mkMove "Ember" Fire Special 40 100 25,
    mkMove "Flamethrower" Fire Special 90 100 15,
    mkMove "Fire Blast" Fire Special 110 85 5,
    mkMove "Fire Spin" Fire Special 35 85 15,
    mkMove "Fire Punch" Fire Physical 75 100 15,
    -- WATER MOVES
    mkMove "Water Gun" Water Special 40 100 25,
    mkMove "Surf" Water Special 90 100 15,
    mkMove "Hydro Pump" Water Special 110 80 5,
    mkMove "Bubblebeam" Water Special 65 100 20,
    mkMove "Clamp" Water Physical 35 85 15,
    mkMove "Crabhammer" Water Physical 100 90 10,
    mkMove "Waterfall" Water Physical 80 100 15,
    mkMove "Withdraw" Water Status 0 100 40,
    -- GRASS MOVES
    mkMove "Vine Whip" Grass Physical 45 100 25,
    mkMove "Razor Leaf" Grass Physical 55 95 25,
    mkMove "Solarbeam" Grass Special 120 100 10, -- Nota: Solarbeam (sin espacio) en la lista anterior
    mkMove "Mega Drain" Grass Special 40 100 15,
    mkMove "Petal Dance" Grass Special 120 100 10,
    mkMove "Leech Seed" Grass Status 0 90 10,
    mkMove "Sleep Powder" Grass Status 0 75 15,
    mkMove "Stun Spore" Grass Status 0 75 30,
    mkMove "Spore" Grass Status 0 100 15,
    -- ELECTRIC MOVES
    mkMove "Thundershock" Electric Special 40 100 30,
    mkMove "Thunderbolt" Electric Special 90 100 15,
    mkMove "Thunder" Electric Special 110 70 10,
    mkMove "Thunderpunch" Electric Physical 75 100 15,
    mkMove "Thunder Wave" Electric Status 0 90 20,
    -- ICE MOVES
    mkMove "Ice Beam" Ice Special 90 100 10,
    mkMove "Blizzard" Ice Special 110 70 5,
    mkMove "Aurora Beam" Ice Special 65 100 20,
    mkMove "Ice Punch" Ice Physical 75 100 15,
    mkMove "Haze" Ice Status 0 100 30,
    mkMove "Mist" Ice Status 0 100 30,
    -- FIGHTING MOVES
    mkMove "Counter" Fighting Physical 1 100 20, -- Variable damage
    mkMove "Seismic Toss" Fighting Physical 1 100 20, -- Fixed damage
    mkMove "Submission" Fighting Physical 80 80 20,
    mkMove "Double Kick" Fighting Physical 30 100 30,
    mkMove "Low Kick" Fighting Physical 50 100 20, -- Weight based usually
    mkMove "Hi Jump Kick" Fighting Physical 130 90 10,
    mkMove "Rolling Kick" Fighting Physical 60 85 15,
    -- POISON MOVES
    mkMove "Toxic" Poison Status 0 85 10,
    mkMove "Sludge" Poison Special 65 100 20,
    mkMove "Acid" Poison Physical 40 100 30,
    mkMove "Smog" Poison Special 30 70 20,
    mkMove "Poisonpowder" Poison Status 0 75 35,
    mkMove "Acid Armor" Poison Status 0 100 20,
    -- GROUND MOVES
    mkMove "Earthquake" Ground Physical 100 100 10,
    mkMove "Dig" Ground Physical 80 100 10,
    mkMove "Bonemerang" Ground Physical 50 90 10,
    mkMove "Bone Club" Ground Physical 65 85 20,
    mkMove "Sand-attack" Ground Status 0 100 15,
    mkMove "Fissure" Ground Physical 1 30 5, -- OHKO

    -- FLYING MOVES
    mkMove "Wing Attack" Flying Physical 60 100 35,
    mkMove "Fly" Flying Physical 90 95 15,
    mkMove "Drill Peck" Flying Physical 80 100 20,
    mkMove "Sky Attack" Flying Physical 140 90 5,
    mkMove "Peck" Flying Physical 35 100 35,
    mkMove "Mirror Move" Flying Status 0 100 20,
    -- PSYCHIC MOVES
    mkMove "Confusion" Psychic Special 50 100 25,
    mkMove "Psychic" Psychic Special 90 100 10,
    mkMove "Psybeam" Psychic Special 65 100 20,
    mkMove "Dream Eater" Psychic Special 100 100 15,
    mkMove "Psystrike" Psychic Special 100 100 10,
    mkMove "Psycho Cut" Psychic Physical 70 100 20,
    mkMove "Rest" Psychic Status 0 100 10,
    mkMove "Hypnosis" Psychic Status 0 60 20,
    mkMove "Reflect" Psychic Status 0 100 20,
    mkMove "Light Screen" Psychic Status 0 100 30,
    mkMove "Barrier" Psychic Status 0 100 20,
    mkMove "Amnesia" Psychic Status 0 100 20,
    mkMove "Meditate" Psychic Status 0 100 40,
    mkMove "Agility" Psychic Status 0 100 30,
    mkMove "Teleport" Psychic Status 0 100 20,
    mkMove "Power Swap" Psychic Status 0 100 10,
    mkMove "Guard Swap" Psychic Status 0 100 10,
    -- ROCK MOVES
    mkMove "Rock Slide" Rock Physical 75 90 10,
    mkMove "Rock Throw" Rock Physical 50 90 15,
    -- GHOST MOVES
    mkMove "Night Shade" Ghost Special 1 100 15, -- Level based dmg
    mkMove "Lick" Ghost Physical 30 100 30,
    mkMove "Confuse Ray" Ghost Status 0 100 10,
    -- BUG MOVES
    mkMove "Pin Missile" Bug Physical 25 95 20,
    mkMove "Twineedle" Bug Physical 25 100 20,
    mkMove "Leech Life" Bug Physical 20 100 15,
    mkMove "String Shot" Bug Status 0 95 40,
    -- DRAGON MOVES
    mkMove "Dragon Claw" Dragon Physical 80 100 15,
    mkMove "Dragon Rage" Dragon Special 1 100 10, -- Fixed dmg 40

    -- DARK MOVES (Retrofitted for Gen 1 Pokemon mostly)
    mkMove "Bite" Dark Physical 60 100 25
  ]

-- Buscar movimiento por nombre
getMoveByName :: String -> Move
getMoveByName name =
  case filter (\m -> mName m == name) allMoves of
    (x : _) -> x
    [] -> mkMove ("MISSING: " ++ name) Normal Physical 10 100 35