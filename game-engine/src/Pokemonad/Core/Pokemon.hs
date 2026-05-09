module Pokemonad.Core.Pokemon
  ( Pokemon (..),
    allPokemon,
    getPokemonById,
  )
where

import Pokemonad.Core.Move (Move, getMoveByName)
import Pokemonad.Core.Types (PokemonId (..), PokemonType (..), Stats (..))

data Pokemon = Pokemon
  { pokemonId :: PokemonId,
    pokemonName :: String,
    pokemonTypes :: [PokemonType],
    pokemonStats :: Stats,
    pokemonDescription :: String,
    pokemonMoves :: [Move]
  }
  deriving (Show, Eq)

allPokemon :: [Pokemon]
allPokemon =
  [ -- ==========================================
    -- STARTERS KANTO (Grass)
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 1,
        pokemonName = "BULBASAUR",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 45 49 49 65 65 45,
        pokemonDescription = "A strange seed was planted on its back at birth.",
        pokemonMoves = map getMoveByName ["Leech Seed", "Toxic", "Body Slam", "Razor Leaf"]
      },
    Pokemon
      { pokemonId = PokemonId 2,
        pokemonName = "IVYSAUR",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 60 62 63 80 80 60,
        pokemonDescription = "Exposure to sunlight adds to its strength.",
        pokemonMoves = map getMoveByName ["Razor Leaf", "Sleep Powder", "Growth", "Double-edge"]
      },
    Pokemon
      { pokemonId = PokemonId 3,
        pokemonName = "VENUSAUR",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 80 82 83 100 100 80,
        pokemonDescription = "The flower on its back catches the sun's rays.",
        pokemonMoves = map getMoveByName ["Leech Seed", "Poisonpowder", "Solarbeam", "Take Down"]
      },
    -- ==========================================
    -- STARTERS KANTO (Fire)
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 4,
        pokemonName = "CHARMANDER",
        pokemonTypes = [Fire],
        pokemonStats = Stats 39 52 43 60 50 65,
        pokemonDescription = "Obviously prefers hot places.",
        pokemonMoves = map getMoveByName ["Flamethrower", "Slash", "Dig", "Fire Spin"]
      },
    Pokemon
      { pokemonId = PokemonId 5,
        pokemonName = "CHARMELEON",
        pokemonTypes = [Fire],
        pokemonStats = Stats 58 64 58 80 65 80,
        pokemonDescription = "When it swings its burning tail, it elevates the temperature.",
        pokemonMoves = map getMoveByName ["Flamethrower", "Counter", "Seismic Toss", "Stun Spore"]
      },
    Pokemon
      { pokemonId = PokemonId 6,
        pokemonName = "CHARIZARD",
        pokemonTypes = [Fire, Flying],
        pokemonStats = Stats 78 84 78 109 85 100,
        pokemonDescription = "Spits fire that is hot enough to melt boulders.",
        pokemonMoves = map getMoveByName ["Fly", "Swords Dance", "Fire Spin", "Fire Blast"]
      },
    -- ==========================================
    -- STARTERS KANTO (Water)
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 7,
        pokemonName = "SQUIRTLE",
        pokemonTypes = [Water],
        pokemonStats = Stats 44 48 65 50 64 43,
        pokemonDescription = "Shoots water at prey while it is in the water.",
        pokemonMoves = map getMoveByName ["Surf", "Blizzard", "Body Slam", "Dig"]
      },
    Pokemon
      { pokemonId = PokemonId 8,
        pokemonName = "WARTORTLE",
        pokemonTypes = [Water],
        pokemonStats = Stats 59 63 80 65 80 58,
        pokemonDescription = "Often hides in water to stalk unwary prey.",
        pokemonMoves = map getMoveByName ["Surf", "Strength", "Rest", "Ice Beam"]
      },
    Pokemon
      { pokemonId = PokemonId 9,
        pokemonName = "BLASTOISE",
        pokemonTypes = [Water],
        pokemonStats = Stats 79 83 100 85 105 78,
        pokemonDescription = "The jets of water it spouts from the rocket cannons.",
        pokemonMoves = map getMoveByName ["Hydro Pump", "Skull Bash", "Withdraw", "Seismic Toss"]
      },
    -- ==========================================
    -- BUGS (Caterpie & Weedle lines)
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 10,
        pokemonName = "CATERPIE",
        pokemonTypes = [Bug],
        pokemonStats = Stats 45 30 35 20 20 45,
        pokemonDescription = "Its short feet are tipped with suction pads.",
        pokemonMoves = map getMoveByName ["String Shot", "Tackle"]
      },
    Pokemon
      { pokemonId = PokemonId 11,
        pokemonName = "METAPOD",
        pokemonTypes = [Bug],
        pokemonStats = Stats 50 20 55 25 25 30,
        pokemonDescription = "This Pokemon is vulnerable to attack while its shell is soft.",
        pokemonMoves = map getMoveByName ["String Shot", "Tackle"]
      },
    Pokemon
      { pokemonId = PokemonId 12,
        pokemonName = "BUTTERFREE",
        pokemonTypes = [Bug, Flying],
        pokemonStats = Stats 60 45 50 90 80 70,
        pokemonDescription = "In battle, it flaps its wings at high speed.",
        pokemonMoves = map getMoveByName ["Psychic", "Supersonic", "Mega Drain", "Stun Spore"]
      },
    Pokemon
      { pokemonId = PokemonId 13,
        pokemonName = "WEEDLE",
        pokemonTypes = [Bug, Poison],
        pokemonStats = Stats 40 35 30 20 20 50,
        pokemonDescription = "Often found in forests, eating leaves.",
        pokemonMoves = map getMoveByName ["String Shot", "Poison Sting"]
      },
    Pokemon
      { pokemonId = PokemonId 14,
        pokemonName = "KAKUNA",
        pokemonTypes = [Bug, Poison],
        pokemonStats = Stats 45 25 50 25 25 35,
        pokemonDescription = "Almost incapable of moving, this Pokemon can only harden its shell.",
        pokemonMoves = map getMoveByName ["String Shot", "Poison Sting"]
      },
    Pokemon
      { pokemonId = PokemonId 15,
        pokemonName = "BEEDRILL",
        pokemonTypes = [Bug, Poison],
        pokemonStats = Stats 65 90 40 45 80 75,
        pokemonDescription = "Flies at high speed and attacks using its large venomous stingers.",
        pokemonMoves = map getMoveByName ["Twineedle", "Hyper Beam", "Toxic", "Focus Energy"]
      },
    -- ==========================================
    -- BIRDS (Pidgey line)
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 16,
        pokemonName = "PIDGEY",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 40 45 40 35 35 56,
        pokemonDescription = "A common sight in forests and woods.",
        pokemonMoves = map getMoveByName ["Fly", "Toxic", "Double-edge", "Double Team"]
      },
    Pokemon
      { pokemonId = PokemonId 17,
        pokemonName = "PIDGEOTTO",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 63 60 55 50 50 71,
        pokemonDescription = "Very protective of its sprawling territorial area.",
        pokemonMoves = map getMoveByName ["Fly", "Quick Attack", "Sand-attack", "Take Down"]
      },
    Pokemon
      { pokemonId = PokemonId 18,
        pokemonName = "PIDGEOT",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 83 80 75 70 70 101,
        pokemonDescription = "When hunting, it skims the surface of water.",
        pokemonMoves = map getMoveByName ["Mirror Move", "Fly", "Quick Attack", "Sand-attack"]
      },
    -- ==========================================
    -- RATS (Rattata line)
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 19,
        pokemonName = "RATTATA",
        pokemonTypes = [Normal],
        pokemonStats = Stats 30 56 35 25 35 72,
        pokemonDescription = "Bites anything when it attacks.",
        pokemonMoves = map getMoveByName ["Super Fang", "Blizzard", "Quick Attack", "Hyper Fang"]
      },
    Pokemon
      { pokemonId = PokemonId 20,
        pokemonName = "RATICATE",
        pokemonTypes = [Normal],
        pokemonStats = Stats 55 81 60 50 70 97,
        pokemonDescription = "It uses its whiskers to maintain its balance.",
        pokemonMoves = map getMoveByName ["Hyper Fang", "Hyper Beam", "Focus Energy", "Thunder"]
      },
    -- ==========================================
    -- SPEAROW LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 21,
        pokemonName = "SPEAROW",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 40 60 30 31 31 70,
        pokemonDescription = "Eats bugs in grassy areas.",
        pokemonMoves = map getMoveByName ["Drill Peck", "Mirror Move", "Double Team", "Double-edge"]
      },
    Pokemon
      { pokemonId = PokemonId 22,
        pokemonName = "FEAROW",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 65 90 65 61 61 100,
        pokemonDescription = "Huge wings can carry it all day.",
        pokemonMoves = map getMoveByName ["Drill Peck", "Mirror Move", "Fury Attack", "Swift"]
      },
    -- ==========================================
    -- SNAKES (Ekans)
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 23,
        pokemonName = "EKANS",
        pokemonTypes = [Poison],
        pokemonStats = Stats 35 60 44 40 54 55,
        pokemonDescription = "Moves silently and stealthily.",
        pokemonMoves = map getMoveByName ["Earthquake", "Acid", "Screech", "Body Slam"]
      },
    Pokemon
      { pokemonId = PokemonId 24,
        pokemonName = "ARBOK",
        pokemonTypes = [Poison],
        pokemonStats = Stats 60 95 69 65 79 80,
        pokemonDescription = "The pattern on its belly appears to be a face.",
        pokemonMoves = map getMoveByName ["Glare", "Wrap", "Dig", "Strength"]
      },
    -- ==========================================
    -- PIKACHU LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 25,
        pokemonName = "PIKACHU",
        pokemonTypes = [Electric],
        pokemonStats = Stats 35 55 40 50 50 90,
        pokemonDescription = "When several of these gather, their electricity could build and cause lightning storms.",
        pokemonMoves = map getMoveByName ["Thunderbolt", "Slam", "Thunder Wave", "Seismic Toss"]
      },
    Pokemon
      { pokemonId = PokemonId 26,
        pokemonName = "RAICHU",
        pokemonTypes = [Electric],
        pokemonStats = Stats 60 90 55 90 80 110,
        pokemonDescription = "Its long tail serves as a ground to protect itself.",
        pokemonMoves = map getMoveByName ["Thunder", "Thunder Wave", "Flash", "Mega Kick"]
      },
    -- ==========================================
    -- SANDSHREW LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 27,
        pokemonName = "SANDSHREW",
        pokemonTypes = [Ground],
        pokemonStats = Stats 50 75 85 20 30 40,
        pokemonDescription = "Burrows deep underground.",
        pokemonMoves = map getMoveByName ["Earthquake", "Slash", "Seismic Toss", "Sand-attack"]
      },
    Pokemon
      { pokemonId = PokemonId 28,
        pokemonName = "SANDSLASH",
        pokemonTypes = [Ground],
        pokemonStats = Stats 75 100 110 45 55 65,
        pokemonDescription = "Curls up into a spiny ball.",
        pokemonMoves = map getMoveByName ["Dig", "Swift", "Seismic Toss", "Sand-attack"]
      },
    -- ==========================================
    -- NIDORAN FAMILY
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 29,
        pokemonName = "NIDORAN F",
        pokemonTypes = [Poison],
        pokemonStats = Stats 55 47 52 40 40 41,
        pokemonDescription = "Small and very poisonous.",
        pokemonMoves = map getMoveByName ["Toxic", "Thunderbolt", "Body Slam", "Blizzard"]
      },
    Pokemon
      { pokemonId = PokemonId 30,
        pokemonName = "NIDORINA",
        pokemonTypes = [Poison],
        pokemonStats = Stats 70 62 67 55 55 56,
        pokemonDescription = "The female's horn develops slowly.",
        pokemonMoves = map getMoveByName ["Toxic", "Thunder", "Double-edge", "Ice Beam"]
      },
    Pokemon
      { pokemonId = PokemonId 31,
        pokemonName = "NIDOQUEEN",
        pokemonTypes = [Poison, Ground],
        pokemonStats = Stats 90 92 87 75 85 76,
        pokemonDescription = "Its hard scales provide strong protection.",
        pokemonMoves = map getMoveByName ["Toxic", "Double Kick", "Bite", "Earthquake"]
      },
    Pokemon
      { pokemonId = PokemonId 32,
        pokemonName = "NIDORAN M",
        pokemonTypes = [Poison],
        pokemonStats = Stats 46 57 40 40 40 50,
        pokemonDescription = "Stiffens its ears to sense danger.",
        pokemonMoves = map getMoveByName ["Blizzard", "Body Slam", "Thunderbolt", "Focus Energy"]
      },
    Pokemon
      { pokemonId = PokemonId 33,
        pokemonName = "NIDORINO",
        pokemonTypes = [Poison],
        pokemonStats = Stats 61 72 57 55 55 65,
        pokemonDescription = "An aggressive Pokemon that is quick to attack.",
        pokemonMoves = map getMoveByName ["Double-edge", "Horn Drill", "Focus Energy", "Thunder"]
      },
    Pokemon
      { pokemonId = PokemonId 34,
        pokemonName = "NIDOKING",
        pokemonTypes = [Poison, Ground],
        pokemonStats = Stats 81 102 77 85 75 85,
        pokemonDescription = "One swing of its mighty tail can snap a telephone pole.",
        pokemonMoves = map getMoveByName ["Earthquake", "Horn Drill", "Rage", "Substitute"]
      },
    -- ==========================================
    -- CLEFAIRY LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 35,
        pokemonName = "CLEFAIRY",
        pokemonTypes = [Fairy],
        pokemonStats = Stats 70 45 48 60 65 35,
        pokemonDescription = "Its magical and cute appeal has many admirers.",
        pokemonMoves = map getMoveByName ["Thunderbolt", "Psychic", "Body Slam", "Blizzard"]
      },
    Pokemon
      { pokemonId = PokemonId 36,
        pokemonName = "CLEFABLE",
        pokemonTypes = [Fairy],
        pokemonStats = Stats 95 70 73 95 90 60,
        pokemonDescription = "A timid fairy Pokemon that is rarely seen.",
        pokemonMoves = map getMoveByName ["Sing", "Tri Attack", "Minimize", "Ice Beam"]
      },
    -- ==========================================
    -- VULPIX LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 37,
        pokemonName = "VULPIX",
        pokemonTypes = [Fire],
        pokemonStats = Stats 38 41 40 50 65 65,
        pokemonDescription = "At the time of birth, it has just one tail.",
        pokemonMoves = map getMoveByName ["Flamethrower", "Dig", "Confuse Ray", "Tail Whip"]
      },
    Pokemon
      { pokemonId = PokemonId 38,
        pokemonName = "NINETALES",
        pokemonTypes = [Fire],
        pokemonStats = Stats 73 76 75 81 100 100,
        pokemonDescription = "Very smart and very vengeful.",
        pokemonMoves = map getMoveByName ["Fire Blast", "Skull Bash", "Confuse Ray", "Tail Whip"]
      },
    -- ==========================================
    -- JIGGLYPUFF LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 39,
        pokemonName = "JIGGLYPUFF",
        pokemonTypes = [Normal, Fairy],
        pokemonStats = Stats 115 45 20 45 25 20,
        pokemonDescription = "Uses its eyes to mesmerize opponents.",
        pokemonMoves = map getMoveByName ["Sing", "Body Slam", "Seismic Toss", "Psychic"]
      },
    Pokemon
      { pokemonId = PokemonId 40,
        pokemonName = "WIGGLYTUFF",
        pokemonTypes = [Normal, Fairy],
        pokemonStats = Stats 140 70 45 85 50 45,
        pokemonDescription = "The body is soft and rubbery.",
        pokemonMoves = map getMoveByName ["Sing", "Double-edge", "Submission", "Thunderbolt"]
      },
    -- ==========================================
    -- ZUBAT LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 41,
        pokemonName = "ZUBAT",
        pokemonTypes = [Poison, Flying],
        pokemonStats = Stats 40 45 35 30 40 55,
        pokemonDescription = "Forms colonies in perpetually dark places.",
        pokemonMoves = map getMoveByName ["Confuse Ray", "Mega Drain", "Toxic", "Double-edge"]
      },
    Pokemon
      { pokemonId = PokemonId 42,
        pokemonName = "GOLBAT",
        pokemonTypes = [Poison, Flying],
        pokemonStats = Stats 75 80 70 65 75 90,
        pokemonDescription = "Once it strikes, it will not stop draining energy.",
        pokemonMoves = map getMoveByName ["Confuse Ray", "Mega Drain", "Bite", "Haze"]
      },
    -- ==========================================
    -- ODDISH LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 43,
        pokemonName = "ODDISH",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 45 50 55 75 65 30,
        pokemonDescription = "During the day, it keeps its face buried in the ground.",
        pokemonMoves = map getMoveByName ["Petal Dance", "Toxic", "Mega Drain", "Double-edge"]
      },
    Pokemon
      { pokemonId = PokemonId 44,
        pokemonName = "GLOOM",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 60 65 70 85 75 40,
        pokemonDescription = "The fluid that oozes from its mouth smells awful.",
        pokemonMoves = map getMoveByName ["Petal Dance", "Take Down", "Mega Drain", "Stun Spore"]
      },
    Pokemon
      { pokemonId = PokemonId 45,
        pokemonName = "VILEPLUME",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 75 80 85 110 90 50,
        pokemonDescription = "The larger its petals, the more toxic pollen it contains.",
        pokemonMoves = map getMoveByName ["Petal Dance", "Sleep Powder", "Acid", "Cut"]
      },
    -- ==========================================
    -- PARAS LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 46,
        pokemonName = "PARAS",
        pokemonTypes = [Bug, Grass],
        pokemonStats = Stats 35 70 55 45 55 25,
        pokemonDescription = "Burrows to suck tree roots.",
        pokemonMoves = map getMoveByName ["Spore", "Slash", "Dig", "Mega Drain"]
      },
    Pokemon
      { pokemonId = PokemonId 47,
        pokemonName = "PARASECT",
        pokemonTypes = [Bug, Grass],
        pokemonStats = Stats 60 95 80 60 80 30,
        pokemonDescription = "A host-parasite pair.",
        pokemonMoves = map getMoveByName ["Spore", "Take Down", "Dig", "Solarbeam"]
      },
    -- ==========================================
    -- VENONAT LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 48,
        pokemonName = "VENONAT",
        pokemonTypes = [Bug, Poison],
        pokemonStats = Stats 60 55 50 40 55 45,
        pokemonDescription = "Lives in the shadows of tall trees.",
        pokemonMoves = map getMoveByName ["Psychic", "Mega Drain", "Double-edge", "Stun Spore"]
      },
    Pokemon
      { pokemonId = PokemonId 49,
        pokemonName = "VENOMOTH",
        pokemonTypes = [Bug, Poison],
        pokemonStats = Stats 70 65 60 90 75 90,
        pokemonDescription = "The dust-like scales cover its wings.",
        pokemonMoves = map getMoveByName ["Psychic", "Supersonic", "Solarbeam", "Swift"]
      },
    -- ==========================================
    -- DIGLETT LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 50,
        pokemonName = "DIGLETT",
        pokemonTypes = [Ground],
        pokemonStats = Stats 10 55 25 35 45 95,
        pokemonDescription = "Lives about one yard underground.",
        pokemonMoves = map getMoveByName ["Earthquake", "Slash", "Sand-attack", "Rock Slide"]
      },
    Pokemon
      { pokemonId = PokemonId 51,
        pokemonName = "DUGTRIO",
        pokemonTypes = [Ground],
        pokemonStats = Stats 35 100 50 50 70 120,
        pokemonDescription = "A team of Diglett triplets.",
        pokemonMoves = map getMoveByName ["Dig", "Sand-attack", "Toxic", "Hyper Beam"]
      },
    -- ==========================================
    -- MEOWTH LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 52,
        pokemonName = "MEOWTH",
        pokemonTypes = [Normal],
        pokemonStats = Stats 40 45 35 40 40 90,
        pokemonDescription = "Adores circular objects.",
        pokemonMoves = map getMoveByName ["Slash", "Thunderbolt", "Swift", "Double Team"]
      },
    Pokemon
      { pokemonId = PokemonId 53,
        pokemonName = "PERSIAN",
        pokemonTypes = [Normal],
        pokemonStats = Stats 65 70 60 65 65 115,
        pokemonDescription = "Many adore it for its sophisticated air.",
        pokemonMoves = map getMoveByName ["Slash", "Bubblebeam", "Mimic", "Growl"]
      },
    -- ==========================================
    -- PSYDUCK LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 54,
        pokemonName = "PSYDUCK",
        pokemonTypes = [Water],
        pokemonStats = Stats 50 52 48 65 50 55,
        pokemonDescription = "While lulling its enemies with its vacant look.",
        pokemonMoves = map getMoveByName ["Surf", "Confusion", "Dig", "Blizzard"]
      },
    Pokemon
      { pokemonId = PokemonId 55,
        pokemonName = "GOLDUCK",
        pokemonTypes = [Water],
        pokemonStats = Stats 80 82 78 95 80 85,
        pokemonDescription = "Often seen swimming elegantly.",
        pokemonMoves = map getMoveByName ["Ice Beam", "Surf", "Toxic", "Disable"]
      },
    -- ==========================================
    -- MANKEY LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 56,
        pokemonName = "MANKEY",
        pokemonTypes = [Fighting],
        pokemonStats = Stats 40 80 35 35 45 70,
        pokemonDescription = "Extremely quick to anger.",
        pokemonMoves = map getMoveByName ["Submission", "Rock Slide", "Seismic Toss", "Screech"]
      },
    Pokemon
      { pokemonId = PokemonId 57,
        pokemonName = "PRIMEAPE",
        pokemonTypes = [Fighting],
        pokemonStats = Stats 65 105 60 60 70 95,
        pokemonDescription = "Always furious and tenacious.",
        pokemonMoves = map getMoveByName ["Fury Swipes", "Rock Slide", "Low Kick", "Screech"]
      },
    -- ==========================================
    -- GROWLITHE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 58,
        pokemonName = "GROWLITHE",
        pokemonTypes = [Fire],
        pokemonStats = Stats 55 70 45 70 50 60,
        pokemonDescription = "Very protective of its territory.",
        pokemonMoves = map getMoveByName ["Flamethrower", "Body Slam", "Reflect", "Dig"]
      },
    Pokemon
      { pokemonId = PokemonId 59,
        pokemonName = "ARCANINE",
        pokemonTypes = [Fire],
        pokemonStats = Stats 90 110 80 100 80 95,
        pokemonDescription = "A Pokemon that has been admired since the past.",
        pokemonMoves = map getMoveByName ["Fire Blast", "Take Down", "Dragon Rage", "Substitute"]
      },
    -- ==========================================
    -- POLIWAG LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 60,
        pokemonName = "POLIWAG",
        pokemonTypes = [Water],
        pokemonStats = Stats 40 50 40 40 40 90,
        pokemonDescription = "Its slick black skin is thin and damp.",
        pokemonMoves = map getMoveByName ["Body Slam", "Blizzard", "Surf", "Amnesia"]
      },
    Pokemon
      { pokemonId = PokemonId 61,
        pokemonName = "POLIWHIRL",
        pokemonTypes = [Water],
        pokemonStats = Stats 65 65 65 50 50 90,
        pokemonDescription = "Capable of living in or out of water.",
        pokemonMoves = map getMoveByName ["Hypnosis", "Surf", "Ice Beam", "Earthquake"]
      },
    Pokemon
      { pokemonId = PokemonId 62,
        pokemonName = "POLIWRATH",
        pokemonTypes = [Water, Fighting],
        pokemonStats = Stats 90 95 95 70 90 70,
        pokemonDescription = "An adept swimmer.",
        pokemonMoves = map getMoveByName ["Hypnosis", "Submission", "Counter", "Hydro Pump"]
      },
    -- ==========================================
    -- ABRA LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 63,
        pokemonName = "ABRA",
        pokemonTypes = [Psychic],
        pokemonStats = Stats 25 20 15 105 55 90,
        pokemonDescription = "Using its ability to read minds.",
        pokemonMoves = map getMoveByName ["Psychic", "Seismic Toss", "Reflect", "Thunder Wave"]
      },
    Pokemon
      { pokemonId = PokemonId 64,
        pokemonName = "KADABRA",
        pokemonTypes = [Psychic],
        pokemonStats = Stats 40 35 30 120 70 105,
        pokemonDescription = "Emits special alpha waves.",
        pokemonMoves = map getMoveByName ["Psychic", "Counter", "Recover", "Dig"]
      },
    Pokemon
      { pokemonId = PokemonId 65,
        pokemonName = "ALAKAZAM",
        pokemonTypes = [Psychic],
        pokemonStats = Stats 55 50 45 135 95 120,
        pokemonDescription = "Its brain can outperform a supercomputer.",
        pokemonMoves = map getMoveByName ["Psybeam", "Metronome", "Disable", "Tri Attack"]
      },
    -- ==========================================
    -- MACHOP LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 66,
        pokemonName = "MACHOP",
        pokemonTypes = [Fighting],
        pokemonStats = Stats 70 80 50 35 35 35,
        pokemonDescription = "Loves to build its muscles.",
        pokemonMoves = map getMoveByName ["Submission", "Rock Slide", "Earthquake", "Focus Energy"]
      },
    Pokemon
      { pokemonId = PokemonId 67,
        pokemonName = "MACHOKE",
        pokemonTypes = [Fighting],
        pokemonStats = Stats 80 100 70 50 60 45,
        pokemonDescription = "Its muscular body is so powerful.",
        pokemonMoves = map getMoveByName ["Submission", "Strength", "Rock Slide", "Focus Energy"]
      },
    Pokemon
      { pokemonId = PokemonId 68,
        pokemonName = "MACHAMP",
        pokemonTypes = [Fighting],
        pokemonStats = Stats 90 130 80 65 85 55,
        pokemonDescription = "Using its heavy muscles, it throws powerful punches.",
        pokemonMoves = map getMoveByName ["Low Kick", "Strength", "Rock Slide", "Focus Energy"]
      },
    -- ==========================================
    -- BELLSPROUT LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 69,
        pokemonName = "BELLSPROUT",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 50 75 35 70 30 40,
        pokemonDescription = "A carnivorous Pokemon that traps and eats bugs.",
        pokemonMoves = map getMoveByName ["Razor Leaf", "Growth", "Mega Drain", "Stun Spore"]
      },
    Pokemon
      { pokemonId = PokemonId 70,
        pokemonName = "WEEPINBELL",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 65 90 50 85 45 55,
        pokemonDescription = "It spits out Poisonpowder to immobilize the enemy.",
        pokemonMoves = map getMoveByName ["Razor Leaf", "Acid", "Wrap", "Toxic"]
      },
    Pokemon
      { pokemonId = PokemonId 71,
        pokemonName = "VICTREEBEL",
        pokemonTypes = [Grass, Poison],
        pokemonStats = Stats 80 105 65 100 70 70,
        pokemonDescription = "Said to live in huge colonies deep in jungles.",
        pokemonMoves = map getMoveByName ["Solarbeam", "Acid", "Reflect", "Slam"]
      },
    -- ==========================================
    -- TENTACOOL LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 72,
        pokemonName = "TENTACOOL",
        pokemonTypes = [Water, Poison],
        pokemonStats = Stats 40 40 35 50 100 70,
        pokemonDescription = "Drifts in shallow seas.",
        pokemonMoves = map getMoveByName ["Surf", "Supersonic", "Mega Drain", "Blizzard"]
      },
    Pokemon
      { pokemonId = PokemonId 73,
        pokemonName = "TENTACRUEL",
        pokemonTypes = [Water, Poison],
        pokemonStats = Stats 80 70 65 80 120 100,
        pokemonDescription = "The tentacles are normally kept short.",
        pokemonMoves = map getMoveByName ["Acid", "Supersonic", "Hydro Pump", "Cut"]
      },
    -- ==========================================
    -- GEODUDE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 74,
        pokemonName = "GEODUDE",
        pokemonTypes = [Rock, Ground],
        pokemonStats = Stats 40 80 100 30 30 20,
        pokemonDescription = "Found in fields and mountains.",
        pokemonMoves = map getMoveByName ["Earthquake", "Seismic Toss", "Rock Slide", "Explosion"]
      },
    Pokemon
      { pokemonId = PokemonId 75,
        pokemonName = "GRAVELER",
        pokemonTypes = [Rock, Ground],
        pokemonStats = Stats 55 95 115 45 45 35,
        pokemonDescription = "Rolls down slopes to move.",
        pokemonMoves = map getMoveByName ["Earthquake", "Seismic Toss", "Strength", "Selfdestruct"]
      },
    Pokemon
      { pokemonId = PokemonId 76,
        pokemonName = "GOLEM",
        pokemonTypes = [Rock, Ground],
        pokemonStats = Stats 80 120 130 55 65 45,
        pokemonDescription = "Its boulder-like body is extremely hard.",
        pokemonMoves = map getMoveByName ["Dig", "Seismic Toss", "Fire Blast", "Metronome"]
      },
    -- ==========================================
    -- PONYTA LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 77,
        pokemonName = "PONYTA",
        pokemonTypes = [Fire],
        pokemonStats = Stats 50 85 55 65 65 90,
        pokemonDescription = "Its hooves are 10 times harder than diamond.",
        pokemonMoves = map getMoveByName ["Fire Blast", "Agility", "Horn Drill", "Body Slam"]
      },
    Pokemon
      { pokemonId = PokemonId 78,
        pokemonName = "RAPIDASH",
        pokemonTypes = [Fire],
        pokemonStats = Stats 65 100 70 80 80 105,
        pokemonDescription = "Very competitive. It chases anything that moves fast.",
        pokemonMoves = map getMoveByName ["Fire Blast", "Stomp", "Toxic", "Fire Spin"]
      },
    -- ==========================================
    -- SLOWPOKE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 79,
        pokemonName = "SLOWPOKE",
        pokemonTypes = [Water, Psychic],
        pokemonStats = Stats 90 65 65 40 40 15,
        pokemonDescription = "Incredibly slow and dopey.",
        pokemonMoves = map getMoveByName ["Surf", "Psychic", "Thunder Wave", "Amnesia"]
      },
    Pokemon
      { pokemonId = PokemonId 80,
        pokemonName = "SLOWBRO",
        pokemonTypes = [Water, Psychic],
        pokemonStats = Stats 95 75 110 100 80 30,
        pokemonDescription = "The Shellder that attaches to its tail feeds on the host.",
        pokemonMoves = map getMoveByName ["Surf", "Psychic", "Disable", "Withdraw"]
      },
    -- ==========================================
    -- MAGNEMITE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 81,
        pokemonName = "MAGNEMITE",
        pokemonTypes = [Electric, Steel],
        pokemonStats = Stats 25 35 70 95 55 45,
        pokemonDescription = "Uses anti-gravity to stay in the air.",
        pokemonMoves = map getMoveByName ["Thunderbolt", "Thunder Wave", "Supersonic", "Double-edge"]
      },
    Pokemon
      { pokemonId = PokemonId 82,
        pokemonName = "MAGNETON",
        pokemonTypes = [Electric, Steel],
        pokemonStats = Stats 50 60 95 120 70 70,
        pokemonDescription = "Formed by several Magnemites linked together.",
        pokemonMoves = map getMoveByName ["Thunder", "Screech", "Supersonic", "Swift"]
      },
    -- ==========================================
    -- FARFETCH'D
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 83,
        pokemonName = "FARFETCH'D",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 52 90 55 58 62 60,
        pokemonDescription = "The sprig of green onions it holds is its weapon.",
        pokemonMoves = map getMoveByName ["Slash", "Sand-attack", "Toxic", "Fly"]
      },
    -- ==========================================
    -- DODUO LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 84,
        pokemonName = "DODUO",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 35 85 45 35 35 75,
        pokemonDescription = "A bird that makes up for its poor flying with fast footwork.",
        pokemonMoves = map getMoveByName ["Drill Peck", "Tri Attack", "Double Team", "Reflect"]
      },
    Pokemon
      { pokemonId = PokemonId 85,
        pokemonName = "DODRIO",
        pokemonTypes = [Normal, Flying],
        pokemonStats = Stats 60 110 70 60 60 110,
        pokemonDescription = "Uses its three brains to execute complex plans.",
        pokemonMoves = map getMoveByName ["Fly", "Tri Attack", "Agility", "Reflect"]
      },
    -- ==========================================
    -- SEEL LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 86,
        pokemonName = "SEEL",
        pokemonTypes = [Water],
        pokemonStats = Stats 65 45 55 45 70 45,
        pokemonDescription = "The protruding horn on its head is very hard.",
        pokemonMoves = map getMoveByName ["Ice Beam", "Body Slam", "Horn Drill", "Surf"]
      },
    Pokemon
      { pokemonId = PokemonId 87,
        pokemonName = "DEWGONG",
        pokemonTypes = [Water, Ice],
        pokemonStats = Stats 90 70 80 70 95 70,
        pokemonDescription = "Stores thermal energy in its body.",
        pokemonMoves = map getMoveByName ["Aurora Beam", "Headbutt", "Rest", "Surf"]
      },
    -- ==========================================
    -- GRIMER LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 88,
        pokemonName = "GRIMER",
        pokemonTypes = [Poison],
        pokemonStats = Stats 80 80 50 40 50 25,
        pokemonDescription = "Appears in filthy areas.",
        pokemonMoves = map getMoveByName ["Sludge", "Body Slam", "Explosion", "Screech"]
      },
    Pokemon
      { pokemonId = PokemonId 89,
        pokemonName = "MUK",
        pokemonTypes = [Poison],
        pokemonStats = Stats 105 105 75 65 100 50,
        pokemonDescription = "Thickly covered with a filthy, vile sludge.",
        pokemonMoves = map getMoveByName ["Sludge", "Thunderbolt", "Hyper Beam", "Selfdestruct"]
      },
    -- ==========================================
    -- SHELLDER LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 90,
        pokemonName = "SHELLDER",
        pokemonTypes = [Water],
        pokemonStats = Stats 30 65 100 45 25 40,
        pokemonDescription = "Its hard shell repels any kind of attack.",
        pokemonMoves = map getMoveByName ["Surf", "Explosion", "Blizzard", "Tri Attack"]
      },
    Pokemon
      { pokemonId = PokemonId 91,
        pokemonName = "CLOYSTER",
        pokemonTypes = [Water, Ice],
        pokemonStats = Stats 50 95 180 85 45 70,
        pokemonDescription = "When attacked, it launches its horns.",
        pokemonMoves = map getMoveByName ["Clamp", "Spike Cannon", "Ice Beam", "Supersonic"]
      },
    -- ==========================================
    -- GASTLY LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 92,
        pokemonName = "GASTLY",
        pokemonTypes = [Ghost, Poison],
        pokemonStats = Stats 30 35 30 100 35 80,
        pokemonDescription = "Almost invisible, this gaseous Pokemon cloaks the target.",
        pokemonMoves = map getMoveByName ["Hypnosis", "Dream Eater", "Psychic", "Confuse Ray"]
      },
    Pokemon
      { pokemonId = PokemonId 93,
        pokemonName = "HAUNTER",
        pokemonTypes = [Ghost, Poison],
        pokemonStats = Stats 45 50 45 115 55 95,
        pokemonDescription = "Because of its ability to slip through block walls.",
        pokemonMoves = map getMoveByName ["Mega Drain", "Psychic", "Explosion", "Confuse Ray"]
      },
    Pokemon
      { pokemonId = PokemonId 94,
        pokemonName = "GENGAR",
        pokemonTypes = [Ghost, Poison],
        pokemonStats = Stats 60 65 60 130 75 110,
        pokemonDescription = "Under a full moon, this Pokemon likes to mimic the shadows.",
        pokemonMoves = map getMoveByName ["Thunderbolt", "Night Shade", "Hypnosis", "Confuse Ray"]
      },
    -- ==========================================
    -- ONIX
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 95,
        pokemonName = "ONIX",
        pokemonTypes = [Rock, Ground],
        pokemonStats = Stats 35 45 160 30 45 70,
        pokemonDescription = "As it grows, the stone portions of its body harden.",
        pokemonMoves = map getMoveByName ["Earthquake", "Rock Slide", "Strength", "Explosion"]
      },
    -- ==========================================
    -- DROWZEE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 96,
        pokemonName = "DROWZEE",
        pokemonTypes = [Psychic],
        pokemonStats = Stats 60 48 45 43 90 42,
        pokemonDescription = "Puts enemies to sleep then eats their dreams.",
        pokemonMoves = map getMoveByName ["Hypnosis", "Dream Eater", "Psychic", "Tri Attack"]
      },
    Pokemon
      { pokemonId = PokemonId 97,
        pokemonName = "HYPNO",
        pokemonTypes = [Psychic],
        pokemonStats = Stats 85 73 70 73 115 67,
        pokemonDescription = "When it locks eyes with an enemy, it uses Hypnosis.",
        pokemonMoves = map getMoveByName ["Hypnosis", "Headbutt", "Dream Eater", "Meditate"]
      },
    -- ==========================================
    -- KRABBY LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 98,
        pokemonName = "KRABBY",
        pokemonTypes = [Water],
        pokemonStats = Stats 30 105 90 25 25 50,
        pokemonDescription = "Its pincers are not only powerful weapons.",
        pokemonMoves = map getMoveByName ["Crabhammer", "Guillotine", "Double-edge", "Blizzard"]
      },
    Pokemon
      { pokemonId = PokemonId 99,
        pokemonName = "KINGLER",
        pokemonTypes = [Water],
        pokemonStats = Stats 55 130 115 50 50 75,
        pokemonDescription = "The large pincer has 10000 hp crushing power.",
        pokemonMoves = map getMoveByName ["Crabhammer", "Guillotine", "Stomp", "Substitute"]
      },
    -- ==========================================
    -- VOLTORB LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 100,
        pokemonName = "VOLTORB",
        pokemonTypes = [Electric],
        pokemonStats = Stats 40 30 50 55 55 100,
        pokemonDescription = "Usually found in power plants.",
        pokemonMoves = map getMoveByName ["Thunderbolt", "Thunder Wave", "Swift", "Explosion"]
      },
    Pokemon
      { pokemonId = PokemonId 101,
        pokemonName = "ELECTRODE",
        pokemonTypes = [Electric],
        pokemonStats = Stats 60 50 70 80 80 150,
        pokemonDescription = "Stores electrical energy inside its body.",
        pokemonMoves = map getMoveByName ["Thunder", "Thunder Wave", "Swift", "Selfdestruct"]
      },
    -- ==========================================
    -- EXEGGCUTE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 102,
        pokemonName = "EXEGGCUTE",
        pokemonTypes = [Grass, Psychic],
        pokemonStats = Stats 60 40 80 60 45 40,
        pokemonDescription = "Often mistaken for eggs.",
        pokemonMoves = map getMoveByName ["Psychic", "Explosion", "Leech Seed", "Toxic"]
      },
    Pokemon
      { pokemonId = PokemonId 103,
        pokemonName = "EXEGGUTOR",
        pokemonTypes = [Grass, Psychic],
        pokemonStats = Stats 95 95 85 125 75 55,
        pokemonDescription = "Legend says that on rare occasions, one of its heads drops off.",
        pokemonMoves = map getMoveByName ["Mega Drain", "Stun Spore", "Leech Seed", "Egg Bomb"]
      },
    -- ==========================================
    -- CUBONE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 104,
        pokemonName = "CUBONE",
        pokemonTypes = [Ground],
        pokemonStats = Stats 50 50 95 40 50 35,
        pokemonDescription = "Wears the skull of its deceased mother.",
        pokemonMoves = map getMoveByName ["Earthquake", "Submission", "Blizzard", "Strength"]
      },
    Pokemon
      { pokemonId = PokemonId 105,
        pokemonName = "MAROWAK",
        pokemonTypes = [Ground],
        pokemonStats = Stats 60 80 110 50 80 45,
        pokemonDescription = "The bone it holds is its key weapon.",
        pokemonMoves = map getMoveByName ["Bonemerang", "Thrash", "Fire Blast", "Focus Energy"]
      },
    -- ==========================================
    -- HITMONS
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 106,
        pokemonName = "HITMONLEE",
        pokemonTypes = [Fighting],
        pokemonStats = Stats 50 120 53 35 110 87,
        pokemonDescription = "When in a hurry, its legs lengthen progressively.",
        pokemonMoves = map getMoveByName ["Hi Jump Kick", "Mega Kick", "Metronome", "Seismic Toss"]
      },
    Pokemon
      { pokemonId = PokemonId 107,
        pokemonName = "HITMONCHAN",
        pokemonTypes = [Fighting],
        pokemonStats = Stats 50 105 79 35 110 76,
        pokemonDescription = "Its punches slice the air.",
        pokemonMoves = map getMoveByName ["Hypnosis", "Dream Eater", "Psychic", "Confuse Ray"]
      },
    -- ==========================================
    -- LICKITUNG
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 108,
        pokemonName = "LICKITUNG",
        pokemonTypes = [Normal],
        pokemonStats = Stats 90 55 75 60 75 30,
        pokemonDescription = "Its tongue can be extended like a chameleon's.",
        pokemonMoves = map getMoveByName ["Strength", "Blizzard", "Thunder", "Fire Blast"]
      },
    -- ==========================================
    -- KOFFING LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 109,
        pokemonName = "KOFFING",
        pokemonTypes = [Poison],
        pokemonStats = Stats 40 65 95 60 45 35,
        pokemonDescription = "Because it stores several kinds of toxic gases.",
        pokemonMoves = map getMoveByName ["Sludge", "Toxic", "Thunderbolt", "Explosion"]
      },
    Pokemon
      { pokemonId = PokemonId 110,
        pokemonName = "WEEZING",
        pokemonTypes = [Poison],
        pokemonStats = Stats 65 90 120 85 70 60,
        pokemonDescription = "Where two kinds of poison gases meet.",
        pokemonMoves = map getMoveByName ["Sludge", "Hyper Beam", "Fire Blast", "Selfdestruct"]
      },
    -- ==========================================
    -- RHYHORN LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 111,
        pokemonName = "RHYHORN",
        pokemonTypes = [Ground, Rock],
        pokemonStats = Stats 80 85 95 30 30 25,
        pokemonDescription = "Its massive bones are 1000 times harder than human bones.",
        pokemonMoves = map getMoveByName ["Earthquake", "Body Slam", "Rock Slide", "Fire Blast"]
      },
    Pokemon
      { pokemonId = PokemonId 112,
        pokemonName = "RHYDON",
        pokemonTypes = [Ground, Rock],
        pokemonStats = Stats 105 130 120 45 45 40,
        pokemonDescription = "Protected by an armor-like hide.",
        pokemonMoves = map getMoveByName ["Dig", "Strength", "Thunder", "Surf"]
      },
    -- ==========================================
    -- CHANSEY
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 113,
        pokemonName = "CHANSEY",
        pokemonTypes = [Normal],
        pokemonStats = Stats 250 5 5 35 105 50,
        pokemonDescription = "A rare and elusive Pokemon.",
        pokemonMoves = map getMoveByName ["Thunder", "Fire Blast", "Minimize", "Rest"]
      },
    -- ==========================================
    -- TANGELA
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 114,
        pokemonName = "TANGELA",
        pokemonTypes = [Grass],
        pokemonStats = Stats 65 55 115 100 40 60,
        pokemonDescription = "The whole body is swathed with wide vines.",
        pokemonMoves = map getMoveByName ["Mega Drain", "Growth", "Toxic", "Double-edge"]
      },
    -- ==========================================
    -- KANGASKHAN
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 115,
        pokemonName = "KANGASKHAN",
        pokemonTypes = [Normal],
        pokemonStats = Stats 105 95 80 40 80 90,
        pokemonDescription = "The infant rarely ventures out of its mother's pouch.",
        pokemonMoves = map getMoveByName ["Dizzy Punch", "Rock Slide", "Surf", "Thunderbolt"]
      },
    -- ==========================================
    -- HORSEA LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 116,
        pokemonName = "HORSEA",
        pokemonTypes = [Water],
        pokemonStats = Stats 30 40 70 70 25 60,
        pokemonDescription = "Known to shoot down flying bugs with precision.",
        pokemonMoves = map getMoveByName ["Hydro Pump", "Toxic", "Smokescreen", "Ice Beam"]
      },
    Pokemon
      { pokemonId = PokemonId 117,
        pokemonName = "SEADRA",
        pokemonTypes = [Water],
        pokemonStats = Stats 55 65 95 95 45 85,
        pokemonDescription = "Capable of swimming backwards.",
        pokemonMoves = map getMoveByName ["Surf", "Toxic", "Smokescreen", "Swift"]
      },
    -- ==========================================
    -- GOLDEEN LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 118,
        pokemonName = "GOLDEEN",
        pokemonTypes = [Water],
        pokemonStats = Stats 45 67 60 35 50 63,
        pokemonDescription = "Its tail fin billows like an elegant dress.",
        pokemonMoves = map getMoveByName ["Surf", "Supersonic", "Horn Attack", "Ice Beam"]
      },
    Pokemon
      { pokemonId = PokemonId 119,
        pokemonName = "SEAKING",
        pokemonTypes = [Water],
        pokemonStats = Stats 80 92 65 65 80 68,
        pokemonDescription = "In the autumn spawning season, they swim up rivers.",
        pokemonMoves = map getMoveByName ["Surf", "Toxic", "Smokescreen", "Swift"]
      },
    -- ==========================================
    -- STARYU LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 120,
        pokemonName = "STARYU",
        pokemonTypes = [Water],
        pokemonStats = Stats 30 45 55 70 55 85,
        pokemonDescription = "An enigmatic Pokemon that can regenerate.",
        pokemonMoves = map getMoveByName ["Hydro Pump", "Recover", "Thunderbolt", "Psychic"]
      },
    Pokemon
      { pokemonId = PokemonId 121,
        pokemonName = "STARMIE",
        pokemonTypes = [Water, Psychic],
        pokemonStats = Stats 60 75 85 100 85 115,
        pokemonDescription = "Its central core glows with the seven colors of the rainbow.",
        pokemonMoves = map getMoveByName ["Surf", "Thunder", "Swift", "Harden"]
      },
    -- ==========================================
    -- MR. MIME
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 122,
        pokemonName = "MR. MIME",
        pokemonTypes = [Psychic, Fairy],
        pokemonStats = Stats 40 45 65 100 120 90,
        pokemonDescription = "If interrupted while miming, it will slap the offender.",
        pokemonMoves = map getMoveByName ["Barrier", "Psychic", "Metronome", "Seismic Toss"]
      },
    -- ==========================================
    -- SCYTHER
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 123,
        pokemonName = "SCYTHER",
        pokemonTypes = [Bug, Flying],
        pokemonStats = Stats 70 110 80 55 80 105,
        pokemonDescription = "With ninja-like agility and speed.",
        pokemonMoves = map getMoveByName ["Slash", "Wing Attack", "Leer", "Double Team"]
      },
    -- ==========================================
    -- JYNX
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 124,
        pokemonName = "JYNX",
        pokemonTypes = [Ice, Psychic],
        pokemonStats = Stats 65 50 35 115 95 95,
        pokemonDescription = "It rocks its body rhythmically.",
        pokemonMoves = map getMoveByName ["Ice Punch", "Mega Punch", "Psychic", "Lovely Kiss"]
      },
    -- ==========================================
    -- ELECTABUZZ
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 125,
        pokemonName = "ELECTABUZZ",
        pokemonTypes = [Electric],
        pokemonStats = Stats 65 83 57 95 85 105,
        pokemonDescription = "Normally found near power plants.",
        pokemonMoves = map getMoveByName ["Thunderpunch", "Mega Punch", "Psychic", "Thunder Wave"]
      },
    -- ==========================================
    -- MAGMAR
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 126,
        pokemonName = "MAGMAR",
        pokemonTypes = [Fire],
        pokemonStats = Stats 65 95 57 100 85 93,
        pokemonDescription = "Born in the spout of a volcano.",
        pokemonMoves = map getMoveByName ["Fire Punch", "Mega Punch", "Psychic", "Smokescreen"]
      },
    -- ==========================================
    -- PINSIR
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 127,
        pokemonName = "PINSIR",
        pokemonTypes = [Bug],
        pokemonStats = Stats 65 125 100 55 70 85,
        pokemonDescription = "Grips its prey with its pincers.",
        pokemonMoves = map getMoveByName ["Strength", "Harden", "Seismic Toss", "Guillotine"]
      },
    -- ==========================================
    -- TAUROS
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 128,
        pokemonName = "TAUROS",
        pokemonTypes = [Normal],
        pokemonStats = Stats 75 100 95 40 70 110,
        pokemonDescription = "When it targets an enemy, it charges furiously.",
        pokemonMoves = map getMoveByName ["Double-edge", "Fire Blast", "Tail Whip", "Bide"]
      },
    -- ==========================================
    -- MAGIKARP LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 129,
        pokemonName = "MAGIKARP",
        pokemonTypes = [Water],
        pokemonStats = Stats 20 10 55 15 20 80,
        pokemonDescription = "A pathetic Pokemon.",
        pokemonMoves = map getMoveByName ["Splash", "Tackle"]
      },
    Pokemon
      { pokemonId = PokemonId 130,
        pokemonName = "GYARADOS",
        pokemonTypes = [Water, Flying],
        pokemonStats = Stats 95 125 79 60 100 81,
        pokemonDescription = "Huge and vicious.",
        pokemonMoves = map getMoveByName ["Surf", "Dragon Rage", "Bite", "Fire Blast"]
      },
    -- ==========================================
    -- LAPRAS
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 131,
        pokemonName = "LAPRAS",
        pokemonTypes = [Water, Ice],
        pokemonStats = Stats 130 85 80 85 95 60,
        pokemonDescription = "A gentle Pokemon that loves to ferry people.",
        pokemonMoves = map getMoveByName ["Ice Beam", "Solarbeam", "Body Slam", "Sing"]
      },
    -- ==========================================
    -- DITTO
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 132,
        pokemonName = "DITTO",
        pokemonTypes = [Normal],
        pokemonStats = Stats 48 48 48 48 48 48,
        pokemonDescription = "Capable of copying an enemy's genetic code.",
        pokemonMoves = [getMoveByName "Transform"]
      },
    -- ==========================================
    -- EEVEE LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 133,
        pokemonName = "EEVEE",
        pokemonTypes = [Normal],
        pokemonStats = Stats 55 55 50 45 65 55,
        pokemonDescription = "Its genetic code is unstable.",
        pokemonMoves = map getMoveByName ["Body Slam", "Swift", "Sand-attack", "Toxic"]
      },
    Pokemon
      { pokemonId = PokemonId 134,
        pokemonName = "VAPOREON",
        pokemonTypes = [Water],
        pokemonStats = Stats 130 65 60 110 95 65,
        pokemonDescription = "Lives close to water.",
        pokemonMoves = map getMoveByName ["Surf", "Quick Attack", "Sand-attack", "Acid Armor"]
      },
    Pokemon
      { pokemonId = PokemonId 135,
        pokemonName = "JOLTEON",
        pokemonTypes = [Electric],
        pokemonStats = Stats 65 65 60 110 95 130,
        pokemonDescription = "It accumulates negative ions in the atmosphere.",
        pokemonMoves = map getMoveByName ["Thunderbolt", "Pin Missile", "Toxic", "Sand-attack"]
      },
    Pokemon
      { pokemonId = PokemonId 136,
        pokemonName = "FLAREON",
        pokemonTypes = [Fire],
        pokemonStats = Stats 65 130 60 95 110 65,
        pokemonDescription = "It has a flame bag inside its body.",
        pokemonMoves = map getMoveByName ["Fire Blast", "Take Down", "Smog", "Sand-attack"]
      },
    -- ==========================================
    -- PORYGON
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 137,
        pokemonName = "PORYGON",
        pokemonTypes = [Normal],
        pokemonStats = Stats 65 60 70 85 75 40,
        pokemonDescription = "A Pokemon that consists entirely of programming code.",
        pokemonMoves = map getMoveByName ["Tri Attack", "Psychic", "Sharpen", "Conversion"]
      },
    -- ==========================================
    -- FOSSILS
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 138,
        pokemonName = "OMANYTE",
        pokemonTypes = [Rock, Water],
        pokemonStats = Stats 35 40 100 90 55 35,
        pokemonDescription = "Revived from an ancient fossil.",
        pokemonMoves = map getMoveByName ["Surf", "Ice Beam", "Double-edge", "Double Team"]
      },
    Pokemon
      { pokemonId = PokemonId 139,
        pokemonName = "OMASTAR",
        pokemonTypes = [Rock, Water],
        pokemonStats = Stats 70 60 125 115 70 55,
        pokemonDescription = "Its sharp beak rings its mouth.",
        pokemonMoves = map getMoveByName ["Hydro Pump", "Submission", "Spike Cannon", "Withdraw"]
      },
    Pokemon
      { pokemonId = PokemonId 140,
        pokemonName = "KABUTO",
        pokemonTypes = [Rock, Water],
        pokemonStats = Stats 30 80 90 55 45 55,
        pokemonDescription = "A Pokemon that was resurrected from a fossil.",
        pokemonMoves = map getMoveByName ["Hydro Pump", "Blizzard", "Slash", "Double Team"]
      },
    Pokemon
      { pokemonId = PokemonId 141,
        pokemonName = "KABUTOPS",
        pokemonTypes = [Rock, Water],
        pokemonStats = Stats 60 115 105 65 70 80,
        pokemonDescription = "Its sleek shape is perfect for swimming.",
        pokemonMoves = map getMoveByName ["Surf", "Swords Dance", "Mega Kick", "Submission"]
      },
    Pokemon
      { pokemonId = PokemonId 142,
        pokemonName = "AERODACTYL",
        pokemonTypes = [Rock, Flying],
        pokemonStats = Stats 80 105 65 60 75 130,
        pokemonDescription = "A ferocious, prehistoric Pokemon.",
        pokemonMoves = map getMoveByName ["Fly", "Hyper Beam", "Supersonic", "Dragon Rage"]
      },
    -- ==========================================
    -- SNORLAX
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 143,
        pokemonName = "SNORLAX",
        pokemonTypes = [Normal],
        pokemonStats = Stats 160 110 65 65 110 30,
        pokemonDescription = "Very lazy. Just eats and sleeps.",
        pokemonMoves = map getMoveByName ["Mega Kick", "Rock Slide", "Metronome", "Rest"]
      },
    -- ==========================================
    -- LEGENDARY BIRDS
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 144,
        pokemonName = "ARTICUNO",
        pokemonTypes = [Ice, Flying],
        pokemonStats = Stats 90 85 100 95 125 85,
        pokemonDescription = "A legendary bird Pokemon that is said to appear to doomed people.",
        pokemonMoves = map getMoveByName ["Ice Beam", "Sky Attack", "Razor Wind", "Substitute"]
      },
    Pokemon
      { pokemonId = PokemonId 145,
        pokemonName = "ZAPDOS",
        pokemonTypes = [Electric, Flying],
        pokemonStats = Stats 90 90 85 125 90 100,
        pokemonDescription = "A legendary bird Pokemon that is said to appear from clouds.",
        pokemonMoves = map getMoveByName ["Thunderbolt", "Sky Attack", "Thunder Wave", "Flash"]
      },
    Pokemon
      { pokemonId = PokemonId 146,
        pokemonName = "MOLTRES",
        pokemonTypes = [Fire, Flying],
        pokemonStats = Stats 90 100 90 125 85 90,
        pokemonDescription = "Known as the legendary bird of fire.",
        pokemonMoves = map getMoveByName ["Fire Blast", "Fly", "Swift", "Substitute"]
      },
    -- ==========================================
    -- DRAGON LINE
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 147,
        pokemonName = "DRATINI",
        pokemonTypes = [Dragon],
        pokemonStats = Stats 41 64 45 50 50 50,
        pokemonDescription = "Long considered a mythical Pokemon.",
        pokemonMoves = map getMoveByName ["Hyper Beam", "Body Slam", "Thunderbolt", "Thunder Wave"]
      },
    Pokemon
      { pokemonId = PokemonId 148,
        pokemonName = "DRAGONAIR",
        pokemonTypes = [Dragon],
        pokemonStats = Stats 61 84 65 70 70 70,
        pokemonDescription = "A mystical Pokemon that exudes a gentle aura.",
        pokemonMoves = map getMoveByName ["Hyper Beam", "Swift", "Ice Beam", "Thunder Wave"]
      },
    Pokemon
      { pokemonId = PokemonId 149,
        pokemonName = "DRAGONITE",
        pokemonTypes = [Dragon, Flying],
        pokemonStats = Stats 91 134 95 100 100 80,
        pokemonDescription = "An extremely rarely seen marine Pokemon.",
        pokemonMoves = map getMoveByName ["Slam", "Dragon Rage", "Thunder", "Agility"]
      },
    -- ==========================================
    -- LEGENDARY CLONES
    -- ==========================================
    Pokemon
      { pokemonId = PokemonId 150,
        pokemonName = "MEWTWO",
        pokemonTypes = [Psychic],
        pokemonStats = Stats 106 110 90 154 90 130,
        pokemonDescription = "It was created by a scientist after years of horrific gene splicing.",
        pokemonMoves = map getMoveByName ["Psycho Cut", "Power Swap", "Guard Swap", "Recover"]
      },
    Pokemon
      { pokemonId = PokemonId 151,
        pokemonName = "MEW",
        pokemonTypes = [Psychic],
        pokemonStats = Stats 100 100 100 100 100 100,
        pokemonDescription = "So rare that it is still said to be a mirage.",
        pokemonMoves = map getMoveByName ["Psychic", "Metronome", "Mega Punch", "Flash"]
      }
  ]

getPokemonById :: PokemonId -> Maybe Pokemon
getPokemonById targetId =
  if null matches then Nothing else Just (head matches)
  where
    matches = filter (\p -> pokemonId p == targetId) allPokemon
