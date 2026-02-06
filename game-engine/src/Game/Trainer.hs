module Game.Trainer where

-- | Representa un entrenador rival con su equipo
data Trainer = Trainer
    { tId         :: Int
    , tName       :: String
    , tSprite     :: String     -- Ruta al sprite del entrenador
    , tTeamIds    :: [Int]      -- IDs de los 6 pokemon de su equipo
    , tDifficulty :: Float      -- Factor de dificultad (para AI futura)
    } deriving (Show, Eq)

-- ===========================================
-- BASE DE DATOS DE ENTRENADORES
-- ===========================================

-- Lista de todos los entrenadores disponibles
allTrainers :: [Trainer]
allTrainers = 
    [ trainerRed
    , trainerBlue
    , trainerCynthia
    , trainerLance
    , trainerGiovanni
    , trainerMisty
    , trainerBrock
    , trainerRocket
    , trainerPaki
    , randomTrainer
    ]

-- ==========================================
-- ENTRENADORES ICÓNICOS
-- ==========================================

trainerRed :: Trainer
trainerRed = Trainer
    { tId = 1
    , tName = "RED"
    , tSprite = "game-client/assets/trainers/red.png"
    , tTeamIds = [25, 6, 9, 3, 143, 131]  -- Pikachu, Charizard, Blastoise, Venusaur, Snorlax, Lapras
    , tDifficulty = 1.5
    }

trainerBlue :: Trainer
trainerBlue = Trainer
    { tId = 2
    , tName = "BLUE"
    , tSprite = "game-client/assets/trainers/blue.png"
    , tTeamIds = [65, 59, 112, 130, 18, 103]  -- Alakazam, Arcanine, Rhydon, Gyarados, Pidgeot, Exeggutor
    , tDifficulty = 1.2
    }

trainerCynthia :: Trainer
trainerCynthia = Trainer
    { tId = 3
    , tName = "CYNTHIA"
    , tSprite = "game-client/assets/trainers/cynthia.png"
    -- , tTeamIds = [445, 130, 407, 350, 423, 468]  -- Garchomp, Gyarados, Roserade, Milotic, Gastrodon, Togekiss
    , tTeamIds = [149, 130, 45, 134, 142, 39] -- Dragonite, Gyarados, Vileplume, Vaporeon, Aerodactyl, Jigglypuff (Adaptado a Gen 1 por seguridad)
    , tDifficulty = 1.4
    }

trainerLance :: Trainer
trainerLance = Trainer
    { tId = 4
    , tName = "LANCE"
    , tSprite = "game-client/assets/trainers/lance.png"
    , tTeamIds = [149, 149, 149, 130, 142, 6]  -- Triple Dragonite + Gyarados + Aerodactyl + Charizard
    , tDifficulty = 1.1
    }

trainerGiovanni :: Trainer
trainerGiovanni = Trainer
    { tId = 5
    , tName = "GIOVANNI"
    , tSprite = "game-client/assets/trainers/giovanni.png"
    , tTeamIds = [53, 34, 31, 112, 51, 115] -- Persian, Nidoking, Nidoqueen, Rhydon, Dugtrio, Kangaskhan
    , tDifficulty = 1.3
    }

trainerMisty :: Trainer
trainerMisty = Trainer
    { tId = 6
    , tName = "MISTY"
    , tSprite = "game-client/assets/trainers/misty.png"
    , tTeamIds = [121, 130, 131, 134, 9, 119] -- Starmie, Gyarados, Lapras, Vaporeon, Blastoise, Seaking
    , tDifficulty = 1.0
    }

trainerBrock :: Trainer
trainerBrock = Trainer
    { tId = 7
    , tName = "BROCK"
    , tSprite = "game-client/assets/trainers/brock.png"
    , tTeamIds = [95, 76, 112, 139, 141, 142] -- Onix, Golem, Rhydon, Omastar, Kabutops, Aerodactyl
    , tDifficulty = 1.0
    }

trainerRocket :: Trainer
trainerRocket = Trainer
    { tId = 8
    , tName = "ROCKET GRUNT"
    , tSprite = "game-client/assets/trainers/rocketgrunt.png"
    , tTeamIds = [19, 41, 23, 109, 88, 52] -- Rattata, Zubat, Ekans, Koffing, Grimer, Meowth
    , tDifficulty = 0.5
    }

trainerPaki :: Trainer
trainerPaki = Trainer
    { tId = 9
    , tName = "PAKI"
    , tSprite = "game-client/assets/trainers/paki.png"
    , tTeamIds = [150, 151, 149, 143, 65, 145] -- Mewtwo, Mew, Dragonite, Snorlax, Alakazam, Zapdos
    , tDifficulty = 2.0
    }

randomTrainer :: Trainer
randomTrainer = Trainer
    { tId = 0
    , tName = "RANDOM OPPONENT"
    , tSprite = "game-client/assets/trainers/random.png"
    , tTeamIds = []  
    , tDifficulty = 0.8
    }

-- ===========================================
-- HELPERS
-- ===========================================

getTrainerById :: Int -> Maybe Trainer
getTrainerById tid = lookup tid [(tId t, t) | t <- allTrainers]

getTrainerByName :: String -> Maybe Trainer
getTrainerByName name = lookup name [(tName t, t) | t <- allTrainers]