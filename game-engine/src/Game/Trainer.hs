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
    , tDifficulty = 1.5  -- El más difícil
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
    , tTeamIds = [445, 130, 407, 350, 423, 468]  -- Garchomp, Gyarados, Roserade, Milotic, Gastrodon, Togekiss
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

randomTrainer :: Trainer
randomTrainer = Trainer
    { tId = 0
    , tName = "RANDOM OPPONENT"
    , tSprite = "game-client/assets/trainers/random.png"
    , tTeamIds = []  -- Se generará aleatoriamente
    , tDifficulty = 0.8
    }

-- ===========================================
-- HELPERS
-- ===========================================

-- | Busca un entrenador por su ID
getTrainerById :: Int -> Maybe Trainer
getTrainerById tid = lookup tid [(tId t, t) | t <- allTrainers]

-- | Busca un entrenador por su nombre
getTrainerByName :: String -> Maybe Trainer
getTrainerByName name = lookup name [(tName t, t) | t <- allTrainers]
