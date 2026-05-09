module Pokemonad.Core.Trainer
  ( AIDifficulty (..),
    Trainer (..),
    allTrainers,
    getTrainerById,
    getTrainerByName,
  )
where

import Pokemonad.Core.Types (PokemonId (..), TrainerId (..))

data AIDifficulty
  = DifficultyEasy
  | DifficultyMedium
  | DifficultyHard
  | DifficultyExtreme
  deriving (Show, Eq)

data Trainer = Trainer
  { trainerId :: TrainerId,
    trainerName :: String,
    trainerTeam :: [PokemonId],
    trainerDifficulty :: AIDifficulty
  }
  deriving (Show, Eq)

allTrainers :: [Trainer]
allTrainers =
  [ trainerRed,
    trainerBlue,
    trainerCynthia,
    trainerLance,
    trainerGiovanni,
    trainerMisty,
    trainerBrock,
    trainerRocket,
    trainerPaki,
    randomTrainer
  ]

trainerRed :: Trainer
trainerRed =
  Trainer
    { trainerId = TrainerId 1,
      trainerName = "RED",
      trainerTeam = map PokemonId [25, 6, 9, 3, 143, 131],
      trainerDifficulty = DifficultyHard
    }

trainerBlue :: Trainer
trainerBlue =
  Trainer
    { trainerId = TrainerId 2,
      trainerName = "BLUE",
      trainerTeam = map PokemonId [65, 59, 112, 130, 18, 103],
      trainerDifficulty = DifficultyMedium
    }

trainerCynthia :: Trainer
trainerCynthia =
  Trainer
    { trainerId = TrainerId 3,
      trainerName = "CYNTHIA",
      trainerTeam = map PokemonId [149, 130, 45, 134, 142, 39],
      trainerDifficulty = DifficultyHard
    }

trainerLance :: Trainer
trainerLance =
  Trainer
    { trainerId = TrainerId 4,
      trainerName = "LANCE",
      trainerTeam = map PokemonId [149, 149, 149, 130, 142, 6],
      trainerDifficulty = DifficultyMedium
    }

trainerGiovanni :: Trainer
trainerGiovanni =
  Trainer
    { trainerId = TrainerId 5,
      trainerName = "GIOVANNI",
      trainerTeam = map PokemonId [53, 34, 31, 112, 51, 115],
      trainerDifficulty = DifficultyMedium
    }

trainerMisty :: Trainer
trainerMisty =
  Trainer
    { trainerId = TrainerId 6,
      trainerName = "MISTY",
      trainerTeam = map PokemonId [121, 130, 131, 134, 9, 119],
      trainerDifficulty = DifficultyEasy
    }

trainerBrock :: Trainer
trainerBrock =
  Trainer
    { trainerId = TrainerId 7,
      trainerName = "BROCK",
      trainerTeam = map PokemonId [95, 76, 112, 139, 141, 142],
      trainerDifficulty = DifficultyEasy
    }

trainerRocket :: Trainer
trainerRocket =
  Trainer
    { trainerId = TrainerId 8,
      trainerName = "ROCKET GRUNT",
      trainerTeam = map PokemonId [19, 41, 23, 109, 88, 52],
      trainerDifficulty = DifficultyEasy
    }

trainerPaki :: Trainer
trainerPaki =
  Trainer
    { trainerId = TrainerId 9,
      trainerName = "PAKI",
      trainerTeam = map PokemonId [150, 151, 149, 143, 65, 145],
      trainerDifficulty = DifficultyExtreme
    }

randomTrainer :: Trainer
randomTrainer =
  Trainer
    { trainerId = TrainerId 0,
      trainerName = "RANDOM OPPONENT",
      trainerTeam = [],
      trainerDifficulty = DifficultyEasy
    }

getTrainerById :: TrainerId -> Maybe Trainer
getTrainerById tid = lookup tid [(trainerId t, t) | t <- allTrainers]

getTrainerByName :: String -> Maybe Trainer
getTrainerByName name = lookup name [(trainerName t, t) | t <- allTrainers]
