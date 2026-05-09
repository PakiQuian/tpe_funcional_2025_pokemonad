module Main where

import Client.Drawing (loadPngSafe)
import Client.Events (handleWorldInput, handleWorldTick)
import Client.Render (drawWorld)
import Client.State (World (..), initialState)
import Client.Types (NetSubState (..))
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.Char (toLower)
import qualified Data.Map as Map
import Graphics.Gloss (Display (InWindow), Picture, black, loadBMP)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Pokemonad.Core.Pokemon (Pokemon (..), allPokemon)
import Pokemonad.Core.Trainer (Trainer (..), allTrainers)
import Pokemonad.Core.Types (PokemonId (..), TrainerId (..))
import System.Random (initStdGen)

main :: IO ()
main = do
  putStrLn "Loading background/UI pictures..."
  logo <- loadBMP "game-client/assets/images/logo.bmp"
  startBg <- loadBMP "game-client/assets/images/background.bmp"
  menuBg <- loadBMP "game-client/assets/images/main_screen.bmp"
  winnerBg <- loadBMP "game-client/assets/images/winner.bmp"
  loserBg <- loadBMP "game-client/assets/images/loser.bmp"

  bg1 <- loadBMP "game-client/assets/images/battle_bg_1.bmp"
  bg2 <- loadBMP "game-client/assets/images/battle_bg_2.bmp"
  bg3 <- loadBMP "game-client/assets/images/battle_bg_3.bmp"
  bg4 <- loadBMP "game-client/assets/images/battle_bg_4.bmp"
  bg5 <- loadBMP "game-client/assets/images/battle_bg_5.bmp"
  let battleBgs = [bg1, bg2, bg3, bg4, bg5]

  putStrLn "Loading front Pokemon sprites..."
  frontSprites <- loadPokemonFrontSprites allPokemon
  let pokemonFrontSpriteMap = Map.fromList frontSprites
  putStrLn $ show (length frontSprites) ++ " pokemons loaded (front sprites)."

  putStrLn "Loading back Pokemon sprites..."
  backSprites <- loadPokemonBackSprites allPokemon
  let pokemonBackSpriteMap = Map.fromList backSprites
  putStrLn $ show (length backSprites) ++ " pokemons loaded (back sprites)."

  putStrLn "Loading Trainer sprites..."
  trainerSprites <- loadTrainerSprites allTrainers
  let trainerSpriteMap = Map.fromList trainerSprites
  putStrLn $ show (length trainerSprites) ++ " trainers loaded."

  rng <- initStdGen
  putStrLn "Setting up game state..."

  netInQ <- newTQueueIO
  netAsync <- newTVarIO Nothing
  aiAsync <- newTVarIO Nothing

  let window = InWindow "Pokemonad P2P" (1280, 720) (100, 100)
      game0 = initialState startBg menuBg logo winnerBg loserBg pokemonFrontSpriteMap pokemonBackSpriteMap trainerSpriteMap battleBgs rng
      world0 =
        World
          { worldGame = game0,
            netInQueue = netInQ,
            netSubState = NetDisconnected,
            netSocket = Nothing,
            netConnAsync = netAsync,
            aiTrainingAsync = aiAsync
          }

  putStrLn "Starting game loop..."
  playIO window black 30 world0 drawWorld handleWorldInput handleWorldTick

loadPokemonFrontSprites :: [Pokemon] -> IO [(PokemonId, Picture)]
loadPokemonFrontSprites [] = return []
loadPokemonFrontSprites (p : ps) = do
  pic <- loadPngSafe (pokemonFrontSpritePath (pokemonId p))
  rest <- loadPokemonFrontSprites ps
  return ((pokemonId p, pic) : rest)

loadPokemonBackSprites :: [Pokemon] -> IO [(PokemonId, Picture)]
loadPokemonBackSprites [] = return []
loadPokemonBackSprites (p : ps) = do
  pic <- loadPngSafe (pokemonBackSpritePath (pokemonId p))
  rest <- loadPokemonBackSprites ps
  return ((pokemonId p, pic) : rest)

loadTrainerSprites :: [Trainer] -> IO [(TrainerId, Picture)]
loadTrainerSprites [] = return []
loadTrainerSprites (t : ts) = do
  pic <- loadPngSafe (trainerSpritePath t)
  rest <- loadTrainerSprites ts
  return ((trainerId t, pic) : rest)

pokemonFrontSpritePath :: PokemonId -> FilePath
pokemonFrontSpritePath (PokemonId n) = "game-client/assets/pokemon/" ++ pad4 n ++ "_front.png"

pokemonBackSpritePath :: PokemonId -> FilePath
pokemonBackSpritePath (PokemonId n) = "game-client/assets/pokemon/" ++ pad4 n ++ "_back.png"

trainerSpritePath :: Trainer -> FilePath
trainerSpritePath t
  | trainerId t == TrainerId 0 = "game-client/assets/trainers/random.png"
  | otherwise = "game-client/assets/trainers/" ++ map toLower (filter (/= ' ') (trainerName t)) ++ ".png"

pad4 :: Int -> String
pad4 n
  | n < 10 = "000" ++ show n
  | n < 100 = "00" ++ show n
  | n < 1000 = "0" ++ show n
  | otherwise = show n
