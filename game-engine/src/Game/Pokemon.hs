module Game.Pokemon where

data PokemonType 
    = Fire | Water | Grass | Normal | Electric | Bug | Flying 
    | Poison | Ground | Rock | Fighting | Psychic | Ghost 
    | Ice | Dragon | Steel | Fairy
    deriving (Show, Eq)

data Stats = Stats
    { hp             :: Int
    , attack         :: Int
    , defense        :: Int
    , specialAttack  :: Int
    , specialDefense :: Int
    , speed          :: Int
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
    [
    -- ==========================================
    -- STARTERS KANTO (Grass)
    -- ==========================================
      Pokemon { pId = 1, pName = "BULBASAUR", pType = [Grass, Poison], pStats = Stats 45 49 49 65 65 45, pDescription = "A strange seed was planted on its back at birth.", frontSprite = "game-client/assets/pokemon/0001_front.png", backSprite  = "game-client/assets/pokemon/0001_back.png" }
    , Pokemon { pId = 2, pName = "IVYSAUR", pType = [Grass, Poison], pStats = Stats 60 62 63 80 80 60, pDescription = "Exposure to sunlight adds to its strength.", frontSprite = "game-client/assets/pokemon/0002_front.png", backSprite  = "game-client/assets/pokemon/0002_back.png" }
    , Pokemon { pId = 3, pName = "VENUSAUR", pType = [Grass, Poison], pStats = Stats 80 82 83 100 100 80, pDescription = "The flower on its back catches the sun's rays.", frontSprite = "game-client/assets/pokemon/0003_front.png", backSprite  = "game-client/assets/pokemon/0003_back.png" }
    
    -- ==========================================
    -- STARTERS KANTO (Fire)
    -- ==========================================
    , Pokemon { pId = 4, pName = "CHARMANDER", pType = [Fire], pStats = Stats 39 52 43 60 50 65, pDescription = "Obviously prefers hot places.", frontSprite = "game-client/assets/pokemon/0004_front.png", backSprite  = "game-client/assets/pokemon/0004_back.png" }
    , Pokemon { pId = 5, pName = "CHARMELEON", pType = [Fire], pStats = Stats 58 64 58 80 65 80, pDescription = "When it swings its burning tail, it elevates the temperature.", frontSprite = "game-client/assets/pokemon/0005_front.png", backSprite  = "game-client/assets/pokemon/0005_back.png" }
    , Pokemon { pId = 6, pName = "CHARIZARD", pType = [Fire, Flying], pStats = Stats 78 84 78 109 85 100, pDescription = "Spits fire that is hot enough to melt boulders.", frontSprite = "game-client/assets/pokemon/0006_front.png", backSprite  = "game-client/assets/pokemon/0006_back.png" }

    -- ==========================================
    -- STARTERS KANTO (Water)
    -- ==========================================
    , Pokemon { pId = 7, pName = "SQUIRTLE", pType = [Water], pStats = Stats 44 48 65 50 64 43, pDescription = "Shoots water at prey while it is in the water.", frontSprite = "game-client/assets/pokemon/0007_front.png", backSprite  = "game-client/assets/pokemon/0007_back.png" }
    , Pokemon { pId = 8, pName = "WARTORTLE", pType = [Water], pStats = Stats 59 63 80 65 80 58, pDescription = "Often hides in water to stalk unwary prey.", frontSprite = "game-client/assets/pokemon/0008_front.png", backSprite  = "game-client/assets/pokemon/0008_back.png" }
    , Pokemon { pId = 9, pName = "BLASTOISE", pType = [Water], pStats = Stats 79 83 100 85 105 78, pDescription = "The jets of water it spouts from the rocket cannons.", frontSprite = "game-client/assets/pokemon/0009_front.png", backSprite  = "game-client/assets/pokemon/0009_back.png" }

    -- ==========================================
    -- BUGS (Caterpie & Weedle lines)
    -- ==========================================
    , Pokemon { pId = 10, pName = "CATERPIE", pType = [Bug], pStats = Stats 45 30 35 20 20 45, pDescription = "Its short feet are tipped with suction pads.", frontSprite = "game-client/assets/pokemon/0010_front.png", backSprite  = "game-client/assets/pokemon/0010_back.png" }
    , Pokemon { pId = 11, pName = "METAPOD", pType = [Bug], pStats = Stats 50 20 55 25 25 30, pDescription = "This Pokemon is vulnerable to attack while its shell is soft.", frontSprite = "game-client/assets/pokemon/0011_front.png", backSprite  = "game-client/assets/pokemon/0011_back.png" }
    , Pokemon { pId = 12, pName = "BUTTERFREE", pType = [Bug, Flying], pStats = Stats 60 45 50 90 80 70, pDescription = "In battle, it flaps its wings at high speed.", frontSprite = "game-client/assets/pokemon/0012_front.png", backSprite  = "game-client/assets/pokemon/0012_back.png" }
    , Pokemon { pId = 13, pName = "WEEDLE", pType = [Bug, Poison], pStats = Stats 40 35 30 20 20 50, pDescription = "Often found in forests, eating leaves.", frontSprite = "game-client/assets/pokemon/0013_front.png", backSprite  = "game-client/assets/pokemon/0013_back.png" }
    , Pokemon { pId = 14, pName = "KAKUNA", pType = [Bug, Poison], pStats = Stats 45 25 50 25 25 35, pDescription = "Almost incapable of moving, this Pokemon can only harden its shell.", frontSprite = "game-client/assets/pokemon/0014_front.png", backSprite  = "game-client/assets/pokemon/0014_back.png" }
    , Pokemon { pId = 15, pName = "BEEDRILL", pType = [Bug, Poison], pStats = Stats 65 90 40 45 80 75, pDescription = "Flies at high speed and attacks using its large venomous stingers.", frontSprite = "game-client/assets/pokemon/0015_front.png", backSprite  = "game-client/assets/pokemon/0015_back.png" }

    -- ==========================================
    -- BIRDS (Pidgey line)
    -- ==========================================
    , Pokemon { pId = 16, pName = "PIDGEY", pType = [Normal, Flying], pStats = Stats 40 45 40 35 35 56, pDescription = "A common sight in forests and woods.", frontSprite = "game-client/assets/pokemon/0016_front.png", backSprite  = "game-client/assets/pokemon/0016_back.png" }
    , Pokemon { pId = 17, pName = "PIDGEOTTO", pType = [Normal, Flying], pStats = Stats 63 60 55 50 50 71, pDescription = "Very protective of its sprawling territorial area.", frontSprite = "game-client/assets/pokemon/0017_front.png", backSprite  = "game-client/assets/pokemon/0017_back.png" }
    , Pokemon { pId = 18, pName = "PIDGEOT", pType = [Normal, Flying], pStats = Stats 83 80 75 70 70 101, pDescription = "When hunting, it skims the surface of water.", frontSprite = "game-client/assets/pokemon/0018_front.png", backSprite  = "game-client/assets/pokemon/0018_back.png" }

    -- ==========================================
    -- RATS (Rattata line)
    -- ==========================================
    , Pokemon { pId = 19, pName = "RATTATA", pType = [Normal], pStats = Stats 30 56 35 25 35 72, pDescription = "Bites anything when it attacks.", frontSprite = "game-client/assets/pokemon/0019_front.png", backSprite  = "game-client/assets/pokemon/0019_back.png" }
    , Pokemon { pId = 20, pName = "RATICATE", pType = [Normal], pStats = Stats 55 81 60 50 70 97, pDescription = "It uses its whiskers to maintain its balance.", frontSprite = "game-client/assets/pokemon/0020_front.png", backSprite  = "game-client/assets/pokemon/0020_back.png" }
    -- ==========================================
    -- SPEAROW LINE
    -- ==========================================
    , Pokemon { pId = 21, pName = "SPEAROW", pType = [Normal, Flying], pStats = Stats 40 60 30 31 31 70, pDescription = "Eats bugs in grassy areas.", frontSprite = "game-client/assets/pokemon/0021_front.png", backSprite = "game-client/assets/pokemon/0021_back.png" }
    , Pokemon { pId = 22, pName = "FEAROW", pType = [Normal, Flying], pStats = Stats 65 90 65 61 61 100, pDescription = "Huge wings can carry it all day.", frontSprite = "game-client/assets/pokemon/0022_front.png", backSprite = "game-client/assets/pokemon/0022_back.png" }

    -- ==========================================
    -- SNAKES (Ekans)
    -- ==========================================
    , Pokemon { pId = 23, pName = "EKANS", pType = [Poison], pStats = Stats 35 60 44 40 54 55, pDescription = "Moves silently and stealthily.", frontSprite = "game-client/assets/pokemon/0023_front.png", backSprite = "game-client/assets/pokemon/0023_back.png" }
    , Pokemon { pId = 24, pName = "ARBOK", pType = [Poison], pStats = Stats 60 95 69 65 79 80, pDescription = "The pattern on its belly appears to be a face.", frontSprite = "game-client/assets/pokemon/0024_front.png", backSprite = "game-client/assets/pokemon/0024_back.png" }

    -- ==========================================
    -- PIKACHU LINE
    -- ==========================================
    , Pokemon { pId = 25, pName = "PIKACHU", pType = [Electric], pStats = Stats 35 55 40 50 50 90, pDescription = "When several of these gather, their electricity could build and cause lightning storms.", frontSprite = "game-client/assets/pokemon/0025_front.png", backSprite = "game-client/assets/pokemon/0025_back.png" }
    , Pokemon { pId = 26, pName = "RAICHU", pType = [Electric], pStats = Stats 60 90 55 90 80 110, pDescription = "Its long tail serves as a ground to protect itself.", frontSprite = "game-client/assets/pokemon/0026_front.png", backSprite = "game-client/assets/pokemon/0026_back.png" }

    -- ==========================================
    -- SANDSHREW LINE
    -- ==========================================
    , Pokemon { pId = 27, pName = "SANDSHREW", pType = [Ground], pStats = Stats 50 75 85 20 30 40, pDescription = "Burrows deep underground.", frontSprite = "game-client/assets/pokemon/0027_front.png", backSprite = "game-client/assets/pokemon/0027_back.png" }
    , Pokemon { pId = 28, pName = "SANDSLASH", pType = [Ground], pStats = Stats 75 100 110 45 55 65, pDescription = "Curls up into a spiny ball.", frontSprite = "game-client/assets/pokemon/0028_front.png", backSprite = "game-client/assets/pokemon/0028_back.png" }

    -- ==========================================
    -- NIDORAN FAMILY
    -- ==========================================
    , Pokemon { pId = 29, pName = "NIDORAN F", pType = [Poison], pStats = Stats 55 47 52 40 40 41, pDescription = "Small and very poisonous.", frontSprite = "game-client/assets/pokemon/0029_front.png", backSprite = "game-client/assets/pokemon/0029_back.png" }
    , Pokemon { pId = 30, pName = "NIDORINA", pType = [Poison], pStats = Stats 70 62 67 55 55 56, pDescription = "The female's horn develops slowly.", frontSprite = "game-client/assets/pokemon/0030_front.png", backSprite = "game-client/assets/pokemon/0030_back.png" }
    , Pokemon { pId = 31, pName = "NIDOQUEEN", pType = [Poison, Ground], pStats = Stats 90 92 87 75 85 76, pDescription = "Its hard scales provide strong protection.", frontSprite = "game-client/assets/pokemon/0031_front.png", backSprite = "game-client/assets/pokemon/0031_back.png" }
    , Pokemon { pId = 32, pName = "NIDORAN M", pType = [Poison], pStats = Stats 46 57 40 40 40 50, pDescription = "Stiffens its ears to sense danger.", frontSprite = "game-client/assets/pokemon/0032_front.png", backSprite = "game-client/assets/pokemon/0032_back.png" }
    , Pokemon { pId = 33, pName = "NIDORINO", pType = [Poison], pStats = Stats 61 72 57 55 55 65, pDescription = "An aggressive Pokemon that is quick to attack.", frontSprite = "game-client/assets/pokemon/0033_front.png", backSprite = "game-client/assets/pokemon/0033_back.png" }
    , Pokemon { pId = 34, pName = "NIDOKING", pType = [Poison, Ground], pStats = Stats 81 102 77 85 75 85, pDescription = "One swing of its mighty tail can snap a telephone pole.", frontSprite = "game-client/assets/pokemon/0034_front.png", backSprite = "game-client/assets/pokemon/0034_back.png" }

    -- ==========================================
    -- CLEFAIRY LINE
    -- ==========================================
    , Pokemon { pId = 35, pName = "CLEFAIRY", pType = [Fairy], pStats = Stats 70 45 48 60 65 35, pDescription = "Its magical and cute appeal has many admirers.", frontSprite = "game-client/assets/pokemon/0035_front.png", backSprite = "game-client/assets/pokemon/0035_back.png" }
    , Pokemon { pId = 36, pName = "CLEFABLE", pType = [Fairy], pStats = Stats 95 70 73 95 90 60, pDescription = "A timid fairy Pokemon that is rarely seen.", frontSprite = "game-client/assets/pokemon/0036_front.png", backSprite = "game-client/assets/pokemon/0036_back.png" }

    -- ==========================================
    -- VULPIX LINE
    -- ==========================================
    , Pokemon { pId = 37, pName = "VULPIX", pType = [Fire], pStats = Stats 38 41 40 50 65 65, pDescription = "At the time of birth, it has just one tail.", frontSprite = "game-client/assets/pokemon/0037_front.png", backSprite = "game-client/assets/pokemon/0037_back.png" }
    , Pokemon { pId = 38, pName = "NINETALES", pType = [Fire], pStats = Stats 73 76 75 81 100 100, pDescription = "Very smart and very vengeful.", frontSprite = "game-client/assets/pokemon/0038_front.png", backSprite = "game-client/assets/pokemon/0038_back.png" }

    -- ==========================================
    -- JIGGLYPUFF LINE
    -- ==========================================
    , Pokemon { pId = 39, pName = "JIGGLYPUFF", pType = [Normal, Fairy], pStats = Stats 115 45 20 45 25 20, pDescription = "Uses its eyes to mesmerize opponents.", frontSprite = "game-client/assets/pokemon/0039_front.png", backSprite = "game-client/assets/pokemon/0039_back.png" }
    , Pokemon { pId = 40, pName = "WIGGLYTUFF", pType = [Normal, Fairy], pStats = Stats 140 70 45 85 50 45, pDescription = "The body is soft and rubbery.", frontSprite = "game-client/assets/pokemon/0040_front.png", backSprite = "game-client/assets/pokemon/0040_back.png" }

    -- ==========================================
    -- ZUBAT LINE
    -- ==========================================
    , Pokemon { pId = 41, pName = "ZUBAT", pType = [Poison, Flying], pStats = Stats 40 45 35 30 40 55, pDescription = "Forms colonies in perpetually dark places.", frontSprite = "game-client/assets/pokemon/0041_front.png", backSprite = "game-client/assets/pokemon/0041_back.png" }
    , Pokemon { pId = 42, pName = "GOLBAT", pType = [Poison, Flying], pStats = Stats 75 80 70 65 75 90, pDescription = "Once it strikes, it will not stop draining energy.", frontSprite = "game-client/assets/pokemon/0042_front.png", backSprite = "game-client/assets/pokemon/0042_back.png" }

    -- ==========================================
    -- ODDISH LINE
    -- ==========================================
    , Pokemon { pId = 43, pName = "ODDISH", pType = [Grass, Poison], pStats = Stats 45 50 55 75 65 30, pDescription = "During the day, it keeps its face buried in the ground.", frontSprite = "game-client/assets/pokemon/0043_front.png", backSprite = "game-client/assets/pokemon/0043_back.png" }
    , Pokemon { pId = 44, pName = "GLOOM", pType = [Grass, Poison], pStats = Stats 60 65 70 85 75 40, pDescription = "The fluid that oozes from its mouth smells awful.", frontSprite = "game-client/assets/pokemon/0044_front.png", backSprite = "game-client/assets/pokemon/0044_back.png" }
    , Pokemon { pId = 45, pName = "VILEPLUME", pType = [Grass, Poison], pStats = Stats 75 80 85 110 90 50, pDescription = "The larger its petals, the more toxic pollen it contains.", frontSprite = "game-client/assets/pokemon/0045_front.png", backSprite = "game-client/assets/pokemon/0045_back.png" }

    -- ==========================================
    -- PARAS LINE
    -- ==========================================
    , Pokemon { pId = 46, pName = "PARAS", pType = [Bug, Grass], pStats = Stats 35 70 55 45 55 25, pDescription = "Burrows to suck tree roots.", frontSprite = "game-client/assets/pokemon/0046_front.png", backSprite = "game-client/assets/pokemon/0046_back.png" }
    , Pokemon { pId = 47, pName = "PARASECT", pType = [Bug, Grass], pStats = Stats 60 95 80 60 80 30, pDescription = "A host-parasite pair.", frontSprite = "game-client/assets/pokemon/0047_front.png", backSprite = "game-client/assets/pokemon/0047_back.png" }

    -- ==========================================
    -- VENONAT LINE
    -- ==========================================
    , Pokemon { pId = 48, pName = "VENONAT", pType = [Bug, Poison], pStats = Stats 60 55 50 40 55 45, pDescription = "Lives in the shadows of tall trees.", frontSprite = "game-client/assets/pokemon/0048_front.png", backSprite = "game-client/assets/pokemon/0048_back.png" }
    , Pokemon { pId = 49, pName = "VENOMOTH", pType = [Bug, Poison], pStats = Stats 70 65 60 90 75 90, pDescription = "The dust-like scales cover its wings.", frontSprite = "game-client/assets/pokemon/0049_front.png", backSprite = "game-client/assets/pokemon/0049_back.png" }

    -- ==========================================
    -- DIGLETT LINE
    -- ==========================================
    , Pokemon { pId = 50, pName = "DIGLETT", pType = [Ground], pStats = Stats 10 55 25 35 45 95, pDescription = "Lives about one yard underground.", frontSprite = "game-client/assets/pokemon/0050_front.png", backSprite = "game-client/assets/pokemon/0050_back.png" }
    , Pokemon { pId = 51, pName = "DUGTRIO", pType = [Ground], pStats = Stats 35 100 50 50 70 120, pDescription = "A team of Diglett triplets.", frontSprite = "game-client/assets/pokemon/0051_front.png", backSprite = "game-client/assets/pokemon/0051_back.png" }

    -- ==========================================
    -- MEOWTH LINE
    -- ==========================================
    , Pokemon { pId = 52, pName = "MEOWTH", pType = [Normal], pStats = Stats 40 45 35 40 40 90, pDescription = "Adores circular objects.", frontSprite = "game-client/assets/pokemon/0052_front.png", backSprite = "game-client/assets/pokemon/0052_back.png" }
    , Pokemon { pId = 53, pName = "PERSIAN", pType = [Normal], pStats = Stats 65 70 60 65 65 115, pDescription = "Many adore it for its sophisticated air.", frontSprite = "game-client/assets/pokemon/0053_front.png", backSprite = "game-client/assets/pokemon/0053_back.png" }

    -- ==========================================
    -- PSYDUCK LINE
    -- ==========================================
    , Pokemon { pId = 54, pName = "PSYDUCK", pType = [Water], pStats = Stats 50 52 48 65 50 55, pDescription = "While lulling its enemies with its vacant look.", frontSprite = "game-client/assets/pokemon/0054_front.png", backSprite = "game-client/assets/pokemon/0054_back.png" }
    , Pokemon { pId = 55, pName = "GOLDUCK", pType = [Water], pStats = Stats 80 82 78 95 80 85, pDescription = "Often seen swimming elegantly.", frontSprite = "game-client/assets/pokemon/0055_front.png", backSprite = "game-client/assets/pokemon/0055_back.png" }

    -- ==========================================
    -- MANKEY LINE
    -- ==========================================
    , Pokemon { pId = 56, pName = "MANKEY", pType = [Fighting], pStats = Stats 40 80 35 35 45 70, pDescription = "Extremely quick to anger.", frontSprite = "game-client/assets/pokemon/0056_front.png", backSprite = "game-client/assets/pokemon/0056_back.png" }
    , Pokemon { pId = 57, pName = "PRIMEAPE", pType = [Fighting], pStats = Stats 65 105 60 60 70 95, pDescription = "Always furious and tenacious.", frontSprite = "game-client/assets/pokemon/0057_front.png", backSprite = "game-client/assets/pokemon/0057_back.png" }

    -- ==========================================
    -- GROWLITHE LINE
    -- ==========================================
    , Pokemon { pId = 58, pName = "GROWLITHE", pType = [Fire], pStats = Stats 55 70 45 70 50 60, pDescription = "Very protective of its territory.", frontSprite = "game-client/assets/pokemon/0058_front.png", backSprite = "game-client/assets/pokemon/0058_back.png" }
    , Pokemon { pId = 59, pName = "ARCANINE", pType = [Fire], pStats = Stats 90 110 80 100 80 95, pDescription = "A Pokemon that has been admired since the past.", frontSprite = "game-client/assets/pokemon/0059_front.png", backSprite = "game-client/assets/pokemon/0059_back.png" }

    -- ==========================================
    -- POLIWAG LINE
    -- ==========================================
    , Pokemon { pId = 60, pName = "POLIWAG", pType = [Water], pStats = Stats 40 50 40 40 40 90, pDescription = "Its slick black skin is thin and damp.", frontSprite = "game-client/assets/pokemon/0060_front.png", backSprite = "game-client/assets/pokemon/0060_back.png" }
    , Pokemon { pId = 61, pName = "POLIWHIRL", pType = [Water], pStats = Stats 65 65 65 50 50 90, pDescription = "Capable of living in or out of water.", frontSprite = "game-client/assets/pokemon/0061_front.png", backSprite = "game-client/assets/pokemon/0061_back.png" }
    , Pokemon { pId = 62, pName = "POLIWRATH", pType = [Water, Fighting], pStats = Stats 90 95 95 70 90 70, pDescription = "An adept swimmer.", frontSprite = "game-client/assets/pokemon/0062_front.png", backSprite = "game-client/assets/pokemon/0062_back.png" }

    -- ==========================================
    -- ABRA LINE
    -- ==========================================
    , Pokemon { pId = 63, pName = "ABRA", pType = [Psychic], pStats = Stats 25 20 15 105 55 90, pDescription = "Using its ability to read minds.", frontSprite = "game-client/assets/pokemon/0063_front.png", backSprite = "game-client/assets/pokemon/0063_back.png" }
    , Pokemon { pId = 64, pName = "KADABRA", pType = [Psychic], pStats = Stats 40 35 30 120 70 105, pDescription = "Emits special alpha waves.", frontSprite = "game-client/assets/pokemon/0064_front.png", backSprite = "game-client/assets/pokemon/0064_back.png" }
    , Pokemon { pId = 65, pName = "ALAKAZAM", pType = [Psychic], pStats = Stats 55 50 45 135 95 120, pDescription = "Its brain can outperform a supercomputer.", frontSprite = "game-client/assets/pokemon/0065_front.png", backSprite = "game-client/assets/pokemon/0065_back.png" }

    -- ==========================================
    -- MACHOP LINE
    -- ==========================================
    , Pokemon { pId = 66, pName = "MACHOP", pType = [Fighting], pStats = Stats 70 80 50 35 35 35, pDescription = "Loves to build its muscles.", frontSprite = "game-client/assets/pokemon/0066_front.png", backSprite = "game-client/assets/pokemon/0066_back.png" }
    , Pokemon { pId = 67, pName = "MACHOKE", pType = [Fighting], pStats = Stats 80 100 70 50 60 45, pDescription = "Its muscular body is so powerful.", frontSprite = "game-client/assets/pokemon/0067_front.png", backSprite = "game-client/assets/pokemon/0067_back.png" }
    , Pokemon { pId = 68, pName = "MACHAMP", pType = [Fighting], pStats = Stats 90 130 80 65 85 55, pDescription = "Using its heavy muscles, it throws powerful punches.", frontSprite = "game-client/assets/pokemon/0068_front.png", backSprite = "game-client/assets/pokemon/0068_back.png" }

    -- ==========================================
    -- BELLSPROUT LINE
    -- ==========================================
    , Pokemon { pId = 69, pName = "BELLSPROUT", pType = [Grass, Poison], pStats = Stats 50 75 35 70 30 40, pDescription = "A carnivorous Pokemon that traps and eats bugs.", frontSprite = "game-client/assets/pokemon/0069_front.png", backSprite = "game-client/assets/pokemon/0069_back.png" }
    , Pokemon { pId = 70, pName = "WEEPINBELL", pType = [Grass, Poison], pStats = Stats 65 90 50 85 45 55, pDescription = "It spits out Poisonpowder to immobilize the enemy.", frontSprite = "game-client/assets/pokemon/0070_front.png", backSprite = "game-client/assets/pokemon/0070_back.png" }
    , Pokemon { pId = 71, pName = "VICTREEBEL", pType = [Grass, Poison], pStats = Stats 80 105 65 100 70 70, pDescription = "Said to live in huge colonies deep in jungles.", frontSprite = "game-client/assets/pokemon/0071_front.png", backSprite = "game-client/assets/pokemon/0071_back.png" }

    -- ==========================================
    -- TENTACOOL LINE
    -- ==========================================
    , Pokemon { pId = 72, pName = "TENTACOOL", pType = [Water, Poison], pStats = Stats 40 40 35 50 100 70, pDescription = "Drifts in shallow seas.", frontSprite = "game-client/assets/pokemon/0072_front.png", backSprite = "game-client/assets/pokemon/0072_back.png" }
    , Pokemon { pId = 73, pName = "TENTACRUEL", pType = [Water, Poison], pStats = Stats 80 70 65 80 120 100, pDescription = "The tentacles are normally kept short.", frontSprite = "game-client/assets/pokemon/0073_front.png", backSprite = "game-client/assets/pokemon/0073_back.png" }

    -- ==========================================
    -- GEODUDE LINE
    -- ==========================================
    , Pokemon { pId = 74, pName = "GEODUDE", pType = [Rock, Ground], pStats = Stats 40 80 100 30 30 20, pDescription = "Found in fields and mountains.", frontSprite = "game-client/assets/pokemon/0074_front.png", backSprite = "game-client/assets/pokemon/0074_back.png" }
    , Pokemon { pId = 75, pName = "GRAVELER", pType = [Rock, Ground], pStats = Stats 55 95 115 45 45 35, pDescription = "Rolls down slopes to move.", frontSprite = "game-client/assets/pokemon/0075_front.png", backSprite = "game-client/assets/pokemon/0075_back.png" }
    , Pokemon { pId = 76, pName = "GOLEM", pType = [Rock, Ground], pStats = Stats 80 120 130 55 65 45, pDescription = "Its boulder-like body is extremely hard.", frontSprite = "game-client/assets/pokemon/0076_front.png", backSprite = "game-client/assets/pokemon/0076_back.png" }

    -- ==========================================
    -- PONYTA LINE
    -- ==========================================
    , Pokemon { pId = 77, pName = "PONYTA", pType = [Fire], pStats = Stats 50 85 55 65 65 90, pDescription = "Its hooves are 10 times harder than diamond.", frontSprite = "game-client/assets/pokemon/0077_front.png", backSprite = "game-client/assets/pokemon/0077_back.png" }
    , Pokemon { pId = 78, pName = "RAPIDASH", pType = [Fire], pStats = Stats 65 100 70 80 80 105, pDescription = "Very competitive. It chases anything that moves fast.", frontSprite = "game-client/assets/pokemon/0078_front.png", backSprite = "game-client/assets/pokemon/0078_back.png" }

    -- ==========================================
    -- SLOWPOKE LINE
    -- ==========================================
    , Pokemon { pId = 79, pName = "SLOWPOKE", pType = [Water, Psychic], pStats = Stats 90 65 65 40 40 15, pDescription = "Incredibly slow and dopey.", frontSprite = "game-client/assets/pokemon/0079_front.png", backSprite = "game-client/assets/pokemon/0079_back.png" }
    , Pokemon { pId = 80, pName = "SLOWBRO", pType = [Water, Psychic], pStats = Stats 95 75 110 100 80 30, pDescription = "The Shellder that attaches to its tail feeds on the host.", frontSprite = "game-client/assets/pokemon/0080_front.png", backSprite = "game-client/assets/pokemon/0080_back.png" }

    -- ==========================================
    -- MAGNEMITE LINE
    -- ==========================================
    , Pokemon { pId = 81, pName = "MAGNEMITE", pType = [Electric, Steel], pStats = Stats 25 35 70 95 55 45, pDescription = "Uses anti-gravity to stay in the air.", frontSprite = "game-client/assets/pokemon/0081_front.png", backSprite = "game-client/assets/pokemon/0081_back.png" }
    , Pokemon { pId = 82, pName = "MAGNETON", pType = [Electric, Steel], pStats = Stats 50 60 95 120 70 70, pDescription = "Formed by several Magnemites linked together.", frontSprite = "game-client/assets/pokemon/0082_front.png", backSprite = "game-client/assets/pokemon/0082_back.png" }

    -- ==========================================
    -- FARFETCH'D
    -- ==========================================
    , Pokemon { pId = 83, pName = "FARFETCH'D", pType = [Normal, Flying], pStats = Stats 52 90 55 58 62 60, pDescription = "The sprig of green onions it holds is its weapon.", frontSprite = "game-client/assets/pokemon/0083_front.png", backSprite = "game-client/assets/pokemon/0083_back.png" }

    -- ==========================================
    -- DODUO LINE
    -- ==========================================
    , Pokemon { pId = 84, pName = "DODUO", pType = [Normal, Flying], pStats = Stats 35 85 45 35 35 75, pDescription = "A bird that makes up for its poor flying with fast footwork.", frontSprite = "game-client/assets/pokemon/0084_front.png", backSprite = "game-client/assets/pokemon/0084_back.png" }
    , Pokemon { pId = 85, pName = "DODRIO", pType = [Normal, Flying], pStats = Stats 60 110 70 60 60 110, pDescription = "Uses its three brains to execute complex plans.", frontSprite = "game-client/assets/pokemon/0085_front.png", backSprite = "game-client/assets/pokemon/0085_back.png" }

    -- ==========================================
    -- SEEL LINE
    -- ==========================================
    , Pokemon { pId = 86, pName = "SEEL", pType = [Water], pStats = Stats 65 45 55 45 70 45, pDescription = "The protruding horn on its head is very hard.", frontSprite = "game-client/assets/pokemon/0086_front.png", backSprite = "game-client/assets/pokemon/0086_back.png" }
    , Pokemon { pId = 87, pName = "DEWGONG", pType = [Water, Ice], pStats = Stats 90 70 80 70 95 70, pDescription = "Stores thermal energy in its body.", frontSprite = "game-client/assets/pokemon/0087_front.png", backSprite = "game-client/assets/pokemon/0087_back.png" }

    -- ==========================================
    -- GRIMER LINE
    -- ==========================================
    , Pokemon { pId = 88, pName = "GRIMER", pType = [Poison], pStats = Stats 80 80 50 40 50 25, pDescription = "Appears in filthy areas.", frontSprite = "game-client/assets/pokemon/0088_front.png", backSprite = "game-client/assets/pokemon/0088_back.png" }
    , Pokemon { pId = 89, pName = "MUK", pType = [Poison], pStats = Stats 105 105 75 65 100 50, pDescription = "Thickly covered with a filthy, vile sludge.", frontSprite = "game-client/assets/pokemon/0089_front.png", backSprite = "game-client/assets/pokemon/0089_back.png" }

    -- ==========================================
    -- SHELLDER LINE
    -- ==========================================
    , Pokemon { pId = 90, pName = "SHELLDER", pType = [Water], pStats = Stats 30 65 100 45 25 40, pDescription = "Its hard shell repels any kind of attack.", frontSprite = "game-client/assets/pokemon/0090_front.png", backSprite = "game-client/assets/pokemon/0090_back.png" }
    , Pokemon { pId = 91, pName = "CLOYSTER", pType = [Water, Ice], pStats = Stats 50 95 180 85 45 70, pDescription = "When attacked, it launches its horns.", frontSprite = "game-client/assets/pokemon/0091_front.png", backSprite = "game-client/assets/pokemon/0091_back.png" }

    -- ==========================================
    -- GASTLY LINE
    -- ==========================================
    , Pokemon { pId = 92, pName = "GASTLY", pType = [Ghost, Poison], pStats = Stats 30 35 30 100 35 80, pDescription = "Almost invisible, this gaseous Pokemon cloaks the target.", frontSprite = "game-client/assets/pokemon/0092_front.png", backSprite = "game-client/assets/pokemon/0092_back.png" }
    , Pokemon { pId = 93, pName = "HAUNTER", pType = [Ghost, Poison], pStats = Stats 45 50 45 115 55 95, pDescription = "Because of its ability to slip through block walls.", frontSprite = "game-client/assets/pokemon/0093_front.png", backSprite = "game-client/assets/pokemon/0093_back.png" }
    , Pokemon { pId = 94, pName = "GENGAR", pType = [Ghost, Poison], pStats = Stats 60 65 60 130 75 110, pDescription = "Under a full moon, this Pokemon likes to mimic the shadows.", frontSprite = "game-client/assets/pokemon/0094_front.png", backSprite = "game-client/assets/pokemon/0094_back.png" }

    -- ==========================================
    -- ONIX
    -- ==========================================
    , Pokemon { pId = 95, pName = "ONIX", pType = [Rock, Ground], pStats = Stats 35 45 160 30 45 70, pDescription = "As it grows, the stone portions of its body harden.", frontSprite = "game-client/assets/pokemon/0095_front.png", backSprite = "game-client/assets/pokemon/0095_back.png" }

    -- ==========================================
    -- DROWZEE LINE
    -- ==========================================
    , Pokemon { pId = 96, pName = "DROWZEE", pType = [Psychic], pStats = Stats 60 48 45 43 90 42, pDescription = "Puts enemies to sleep then eats their dreams.", frontSprite = "game-client/assets/pokemon/0096_front.png", backSprite = "game-client/assets/pokemon/0096_back.png" }
    , Pokemon { pId = 97, pName = "HYPNO", pType = [Psychic], pStats = Stats 85 73 70 73 115 67, pDescription = "When it locks eyes with an enemy, it uses Hypnosis.", frontSprite = "game-client/assets/pokemon/0097_front.png", backSprite = "game-client/assets/pokemon/0097_back.png" }

    -- ==========================================
    -- KRABBY LINE
    -- ==========================================
    , Pokemon { pId = 98, pName = "KRABBY", pType = [Water], pStats = Stats 30 105 90 25 25 50, pDescription = "Its pincers are not only powerful weapons.", frontSprite = "game-client/assets/pokemon/0098_front.png", backSprite = "game-client/assets/pokemon/0098_back.png" }
    , Pokemon { pId = 99, pName = "KINGLER", pType = [Water], pStats = Stats 55 130 115 50 50 75, pDescription = "The large pincer has 10000 hp crushing power.", frontSprite = "game-client/assets/pokemon/0099_front.png", backSprite = "game-client/assets/pokemon/0099_back.png" }

    -- ==========================================
    -- VOLTORB LINE
    -- ==========================================
    , Pokemon { pId = 100, pName = "VOLTORB", pType = [Electric], pStats = Stats 40 30 50 55 55 100, pDescription = "Usually found in power plants.", frontSprite = "game-client/assets/pokemon/0100_front.png", backSprite = "game-client/assets/pokemon/0100_back.png" }
    , Pokemon { pId = 101, pName = "ELECTRODE", pType = [Electric], pStats = Stats 60 50 70 80 80 150, pDescription = "Stores electrical energy inside its body.", frontSprite = "game-client/assets/pokemon/0101_front.png", backSprite = "game-client/assets/pokemon/0101_back.png" }

    -- ==========================================
    -- EXEGGCUTE LINE
    -- ==========================================
    , Pokemon { pId = 102, pName = "EXEGGCUTE", pType = [Grass, Psychic], pStats = Stats 60 40 80 60 45 40, pDescription = "Often mistaken for eggs.", frontSprite = "game-client/assets/pokemon/0102_front.png", backSprite = "game-client/assets/pokemon/0102_back.png" }
    , Pokemon { pId = 103, pName = "EXEGGUTOR", pType = [Grass, Psychic], pStats = Stats 95 95 85 125 75 55, pDescription = "Legend says that on rare occasions, one of its heads drops off.", frontSprite = "game-client/assets/pokemon/0103_front.png", backSprite = "game-client/assets/pokemon/0103_back.png" }

    -- ==========================================
    -- CUBONE LINE
    -- ==========================================
    , Pokemon { pId = 104, pName = "CUBONE", pType = [Ground], pStats = Stats 50 50 95 40 50 35, pDescription = "Wears the skull of its deceased mother.", frontSprite = "game-client/assets/pokemon/0104_front.png", backSprite = "game-client/assets/pokemon/0104_back.png" }
    , Pokemon { pId = 105, pName = "MAROWAK", pType = [Ground], pStats = Stats 60 80 110 50 80 45, pDescription = "The bone it holds is its key weapon.", frontSprite = "game-client/assets/pokemon/0105_front.png", backSprite = "game-client/assets/pokemon/0105_back.png" }

    -- ==========================================
    -- HITMONS
    -- ==========================================
    , Pokemon { pId = 106, pName = "HITMONLEE", pType = [Fighting], pStats = Stats 50 120 53 35 110 87, pDescription = "When in a hurry, its legs lengthen progressively.", frontSprite = "game-client/assets/pokemon/0106_front.png", backSprite = "game-client/assets/pokemon/0106_back.png" }
    , Pokemon { pId = 107, pName = "HITMONCHAN", pType = [Fighting], pStats = Stats 50 105 79 35 110 76, pDescription = "Its punches slice the air.", frontSprite = "game-client/assets/pokemon/0107_front.png", backSprite = "game-client/assets/pokemon/0107_back.png" }

    -- ==========================================
    -- LICKITUNG
    -- ==========================================
    , Pokemon { pId = 108, pName = "LICKITUNG", pType = [Normal], pStats = Stats 90 55 75 60 75 30, pDescription = "Its tongue can be extended like a chameleon's.", frontSprite = "game-client/assets/pokemon/0108_front.png", backSprite = "game-client/assets/pokemon/0108_back.png" }

    -- ==========================================
    -- KOFFING LINE
    -- ==========================================
    , Pokemon { pId = 109, pName = "KOFFING", pType = [Poison], pStats = Stats 40 65 95 60 45 35, pDescription = "Because it stores several kinds of toxic gases.", frontSprite = "game-client/assets/pokemon/0109_front.png", backSprite = "game-client/assets/pokemon/0109_back.png" }
    , Pokemon { pId = 110, pName = "WEEZING", pType = [Poison], pStats = Stats 65 90 120 85 70 60, pDescription = "Where two kinds of poison gases meet.", frontSprite = "game-client/assets/pokemon/0110_front.png", backSprite = "game-client/assets/pokemon/0110_back.png" }

    -- ==========================================
    -- RHYHORN LINE
    -- ==========================================
    , Pokemon { pId = 111, pName = "RHYHORN", pType = [Ground, Rock], pStats = Stats 80 85 95 30 30 25, pDescription = "Its massive bones are 1000 times harder than human bones.", frontSprite = "game-client/assets/pokemon/0111_front.png", backSprite = "game-client/assets/pokemon/0111_back.png" }
    , Pokemon { pId = 112, pName = "RHYDON", pType = [Ground, Rock], pStats = Stats 105 130 120 45 45 40, pDescription = "Protected by an armor-like hide.", frontSprite = "game-client/assets/pokemon/0112_front.png", backSprite = "game-client/assets/pokemon/0112_back.png" }

    -- ==========================================
    -- CHANSEY
    -- ==========================================
    , Pokemon { pId = 113, pName = "CHANSEY", pType = [Normal], pStats = Stats 250 5 5 35 105 50, pDescription = "A rare and elusive Pokemon.", frontSprite = "game-client/assets/pokemon/0113_front.png", backSprite = "game-client/assets/pokemon/0113_back.png" }

    -- ==========================================
    -- TANGELA
    -- ==========================================
    , Pokemon { pId = 114, pName = "TANGELA", pType = [Grass], pStats = Stats 65 55 115 100 40 60, pDescription = "The whole body is swathed with wide vines.", frontSprite = "game-client/assets/pokemon/0114_front.png", backSprite = "game-client/assets/pokemon/0114_back.png" }

    -- ==========================================
    -- KANGASKHAN
    -- ==========================================
    , Pokemon { pId = 115, pName = "KANGASKHAN", pType = [Normal], pStats = Stats 105 95 80 40 80 90, pDescription = "The infant rarely ventures out of its mother's pouch.", frontSprite = "game-client/assets/pokemon/0115_front.png", backSprite = "game-client/assets/pokemon/0115_back.png" }

    -- ==========================================
    -- HORSEA LINE
    -- ==========================================
    , Pokemon { pId = 116, pName = "HORSEA", pType = [Water], pStats = Stats 30 40 70 70 25 60, pDescription = "Known to shoot down flying bugs with precision.", frontSprite = "game-client/assets/pokemon/0116_front.png", backSprite = "game-client/assets/pokemon/0116_back.png" }
    , Pokemon { pId = 117, pName = "SEADRA", pType = [Water], pStats = Stats 55 65 95 95 45 85, pDescription = "Capable of swimming backwards.", frontSprite = "game-client/assets/pokemon/0117_front.png", backSprite = "game-client/assets/pokemon/0117_back.png" }

    -- ==========================================
    -- GOLDEEN LINE
    -- ==========================================
    , Pokemon { pId = 118, pName = "GOLDEEN", pType = [Water], pStats = Stats 45 67 60 35 50 63, pDescription = "Its tail fin billows like an elegant dress.", frontSprite = "game-client/assets/pokemon/0118_front.png", backSprite = "game-client/assets/pokemon/0118_back.png" }
    , Pokemon { pId = 119, pName = "SEAKING", pType = [Water], pStats = Stats 80 92 65 65 80 68, pDescription = "In the autumn spawning season, they swim up rivers.", frontSprite = "game-client/assets/pokemon/0119_front.png", backSprite = "game-client/assets/pokemon/0119_back.png" }

    -- ==========================================
    -- STARYU LINE
    -- ==========================================
    , Pokemon { pId = 120, pName = "STARYU", pType = [Water], pStats = Stats 30 45 55 70 55 85, pDescription = "An enigmatic Pokemon that can regenerate.", frontSprite = "game-client/assets/pokemon/0120_front.png", backSprite = "game-client/assets/pokemon/0120_back.png" }
    , Pokemon { pId = 121, pName = "STARMIE", pType = [Water, Psychic], pStats = Stats 60 75 85 100 85 115, pDescription = "Its central core glows with the seven colors of the rainbow.", frontSprite = "game-client/assets/pokemon/0121_front.png", backSprite = "game-client/assets/pokemon/0121_back.png" }

    -- ==========================================
    -- MR. MIME
    -- ==========================================
    , Pokemon { pId = 122, pName = "MR. MIME", pType = [Psychic, Fairy], pStats = Stats 40 45 65 100 120 90, pDescription = "If interrupted while miming, it will slap the offender.", frontSprite = "game-client/assets/pokemon/0122_front.png", backSprite = "game-client/assets/pokemon/0122_back.png" }

    -- ==========================================
    -- SCYTHER
    -- ==========================================
    , Pokemon { pId = 123, pName = "SCYTHER", pType = [Bug, Flying], pStats = Stats 70 110 80 55 80 105, pDescription = "With ninja-like agility and speed.", frontSprite = "game-client/assets/pokemon/0123_front.png", backSprite = "game-client/assets/pokemon/0123_back.png" }

    -- ==========================================
    -- JYNX
    -- ==========================================
    , Pokemon { pId = 124, pName = "JYNX", pType = [Ice, Psychic], pStats = Stats 65 50 35 115 95 95, pDescription = "It rocks its body rhythmically.", frontSprite = "game-client/assets/pokemon/0124_front.png", backSprite = "game-client/assets/pokemon/0124_back.png" }

    -- ==========================================
    -- ELECTABUZZ
    -- ==========================================
    , Pokemon { pId = 125, pName = "ELECTABUZZ", pType = [Electric], pStats = Stats 65 83 57 95 85 105, pDescription = "Normally found near power plants.", frontSprite = "game-client/assets/pokemon/0125_front.png", backSprite = "game-client/assets/pokemon/0125_back.png" }

    -- ==========================================
    -- MAGMAR
    -- ==========================================
    , Pokemon { pId = 126, pName = "MAGMAR", pType = [Fire], pStats = Stats 65 95 57 100 85 93, pDescription = "Born in the spout of a volcano.", frontSprite = "game-client/assets/pokemon/0126_front.png", backSprite = "game-client/assets/pokemon/0126_back.png" }

    -- ==========================================
    -- PINSIR
    -- ==========================================
    , Pokemon { pId = 127, pName = "PINSIR", pType = [Bug], pStats = Stats 65 125 100 55 70 85, pDescription = "Grips its prey with its pincers.", frontSprite = "game-client/assets/pokemon/0127_front.png", backSprite = "game-client/assets/pokemon/0127_back.png" }

    -- ==========================================
    -- TAUROS
    -- ==========================================
    , Pokemon { pId = 128, pName = "TAUROS", pType = [Normal], pStats = Stats 75 100 95 40 70 110, pDescription = "When it targets an enemy, it charges furiously.", frontSprite = "game-client/assets/pokemon/0128_front.png", backSprite = "game-client/assets/pokemon/0128_back.png" }

    -- ==========================================
    -- MAGIKARP LINE
    -- ==========================================
    , Pokemon { pId = 129, pName = "MAGIKARP", pType = [Water], pStats = Stats 20 10 55 15 20 80, pDescription = "A pathetic Pokemon.", frontSprite = "game-client/assets/pokemon/0129_front.png", backSprite = "game-client/assets/pokemon/0129_back.png" }
    , Pokemon { pId = 130, pName = "GYARADOS", pType = [Water, Flying], pStats = Stats 95 125 79 60 100 81, pDescription = "Huge and vicious.", frontSprite = "game-client/assets/pokemon/0130_front.png", backSprite = "game-client/assets/pokemon/0130_back.png" }

    -- ==========================================
    -- LAPRAS
    -- ==========================================
    , Pokemon { pId = 131, pName = "LAPRAS", pType = [Water, Ice], pStats = Stats 130 85 80 85 95 60, pDescription = "A gentle Pokemon that loves to ferry people.", frontSprite = "game-client/assets/pokemon/0131_front.png", backSprite = "game-client/assets/pokemon/0131_back.png" }

    -- ==========================================
    -- DITTO
    -- ==========================================
    , Pokemon { pId = 132, pName = "DITTO", pType = [Normal], pStats = Stats 48 48 48 48 48 48, pDescription = "Capable of copying an enemy's genetic code.", frontSprite = "game-client/assets/pokemon/0132_front.png", backSprite = "game-client/assets/pokemon/0132_back.png" }

    -- ==========================================
    -- EEVEE LINE
    -- ==========================================
    , Pokemon { pId = 133, pName = "EEVEE", pType = [Normal], pStats = Stats 55 55 50 45 65 55, pDescription = "Its genetic code is unstable.", frontSprite = "game-client/assets/pokemon/0133_front.png", backSprite = "game-client/assets/pokemon/0133_back.png" }
    , Pokemon { pId = 134, pName = "VAPOREON", pType = [Water], pStats = Stats 130 65 60 110 95 65, pDescription = "Lives close to water.", frontSprite = "game-client/assets/pokemon/0134_front.png", backSprite = "game-client/assets/pokemon/0134_back.png" }
    , Pokemon { pId = 135, pName = "JOLTEON", pType = [Electric], pStats = Stats 65 65 60 110 95 130, pDescription = "It accumulates negative ions in the atmosphere.", frontSprite = "game-client/assets/pokemon/0135_front.png", backSprite = "game-client/assets/pokemon/0135_back.png" }
    , Pokemon { pId = 136, pName = "FLAREON", pType = [Fire], pStats = Stats 65 130 60 95 110 65, pDescription = "It has a flame bag inside its body.", frontSprite = "game-client/assets/pokemon/0136_front.png", backSprite = "game-client/assets/pokemon/0136_back.png" }

    -- ==========================================
    -- PORYGON
    -- ==========================================
    , Pokemon { pId = 137, pName = "PORYGON", pType = [Normal], pStats = Stats 65 60 70 85 75 40, pDescription = "A Pokemon that consists entirely of programming code.", frontSprite = "game-client/assets/pokemon/0137_front.png", backSprite = "game-client/assets/pokemon/0137_back.png" }

    -- ==========================================
    -- FOSSILS
    -- ==========================================
    , Pokemon { pId = 138, pName = "OMANYTE", pType = [Rock, Water], pStats = Stats 35 40 100 90 55 35, pDescription = "Revived from an ancient fossil.", frontSprite = "game-client/assets/pokemon/0138_front.png", backSprite = "game-client/assets/pokemon/0138_back.png" }
    , Pokemon { pId = 139, pName = "OMASTAR", pType = [Rock, Water], pStats = Stats 70 60 125 115 70 55, pDescription = "Its sharp beak rings its mouth.", frontSprite = "game-client/assets/pokemon/0139_front.png", backSprite = "game-client/assets/pokemon/0139_back.png" }
    , Pokemon { pId = 140, pName = "KABUTO", pType = [Rock, Water], pStats = Stats 30 80 90 55 45 55, pDescription = "A Pokemon that was resurrected from a fossil.", frontSprite = "game-client/assets/pokemon/0140_front.png", backSprite = "game-client/assets/pokemon/0140_back.png" }
    , Pokemon { pId = 141, pName = "KABUTOPS", pType = [Rock, Water], pStats = Stats 60 115 105 65 70 80, pDescription = "Its sleek shape is perfect for swimming.", frontSprite = "game-client/assets/pokemon/0141_front.png", backSprite = "game-client/assets/pokemon/0141_back.png" }
    , Pokemon { pId = 142, pName = "AERODACTYL", pType = [Rock, Flying], pStats = Stats 80 105 65 60 75 130, pDescription = "A ferocious, prehistoric Pokemon.", frontSprite = "game-client/assets/pokemon/0142_front.png", backSprite = "game-client/assets/pokemon/0142_back.png" }

    -- ==========================================
    -- SNORLAX
    -- ==========================================
    , Pokemon { pId = 143, pName = "SNORLAX", pType = [Normal], pStats = Stats 160 110 65 65 110 30, pDescription = "Very lazy. Just eats and sleeps.", frontSprite = "game-client/assets/pokemon/0143_front.png", backSprite = "game-client/assets/pokemon/0143_back.png" }

    -- ==========================================
    -- LEGENDARY BIRDS
    -- ==========================================
    , Pokemon { pId = 144, pName = "ARTICUNO", pType = [Ice, Flying], pStats = Stats 90 85 100 95 125 85, pDescription = "A legendary bird Pokemon that is said to appear to doomed people.", frontSprite = "game-client/assets/pokemon/0144_front.png", backSprite = "game-client/assets/pokemon/0144_back.png" }
    , Pokemon { pId = 145, pName = "ZAPDOS", pType = [Electric, Flying], pStats = Stats 90 90 85 125 90 100, pDescription = "A legendary bird Pokemon that is said to appear from clouds.", frontSprite = "game-client/assets/pokemon/0145_front.png", backSprite = "game-client/assets/pokemon/0145_back.png" }
    , Pokemon { pId = 146, pName = "MOLTRES", pType = [Fire, Flying], pStats = Stats 90 100 90 125 85 90, pDescription = "Known as the legendary bird of fire.", frontSprite = "game-client/assets/pokemon/0146_front.png", backSprite = "game-client/assets/pokemon/0146_back.png" }

    -- ==========================================
    -- DRAGON LINE
    -- ==========================================
    , Pokemon { pId = 147, pName = "DRATINI", pType = [Dragon], pStats = Stats 41 64 45 50 50 50, pDescription = "Long considered a mythical Pokemon.", frontSprite = "game-client/assets/pokemon/0147_front.png", backSprite = "game-client/assets/pokemon/0147_back.png" }
    , Pokemon { pId = 148, pName = "DRAGONAIR", pType = [Dragon], pStats = Stats 61 84 65 70 70 70, pDescription = "A mystical Pokemon that exudes a gentle aura.", frontSprite = "game-client/assets/pokemon/0148_front.png", backSprite = "game-client/assets/pokemon/0148_back.png" }
    , Pokemon { pId = 149, pName = "DRAGONITE", pType = [Dragon, Flying], pStats = Stats 91 134 95 100 100 80, pDescription = "An extremely rarely seen marine Pokemon.", frontSprite = "game-client/assets/pokemon/0149_front.png", backSprite = "game-client/assets/pokemon/0149_back.png" }

    -- ==========================================
    -- LEGENDARY CLONES
    -- ==========================================
    , Pokemon { pId = 150, pName = "MEWTWO", pType = [Psychic], pStats = Stats 106 110 90 154 90 130, pDescription = "It was created by a scientist after years of horrific gene splicing.", frontSprite = "game-client/assets/pokemon/0150_front.png", backSprite = "game-client/assets/pokemon/0150_back.png" }
    , Pokemon { pId = 151, pName = "MEW", pType = [Psychic], pStats = Stats 100 100 100 100 100 100, pDescription = "So rare that it is still said to be a mirage.", frontSprite = "game-client/assets/pokemon/0151_front.png", backSprite = "game-client/assets/pokemon/0151_back.png" }
    ]

getPokemonById :: Int -> Maybe Pokemon
getPokemonById targetId = 
    if null matches then Nothing else Just (head matches)
  where 
    matches = filter (\p -> pId p == targetId) allPokemon