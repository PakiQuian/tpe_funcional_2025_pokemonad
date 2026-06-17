-- | Pruebas lógicas del núcleo puro (game-engine).
--
--   El foco no es cobertura exhaustiva, sino validar invariantes y, sobre
--   todo, demostrar que la lógica de combate y el aprendizaje por refuerzo
--   son funciones puras y deterministas: dado un mismo 'StdGen' inicial, el
--   entrenamiento produce exactamente los mismos pesos.
module Main (main) where

import System.Random (mkStdGen)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, arbitrary, elements, listOf1, testProperty)
import qualified Test.Tasty.QuickCheck as QC

import Pokemonad.AI.HyperParams (defaultTrainingHyperParams)
import Pokemonad.AI.Model
  ( QWeights (..),
    candidateActions,
    chooseActionGreedy,
    defaultQWeights,
    qValue,
  )
import Pokemonad.AI.Persistence
  ( AICheckpointData (..),
    decodeCheckpoint,
    encodeCheckpoint,
  )
import Pokemonad.AI.Training (runTrainingEpochs, tdUpdate)
import Pokemonad.Battle.Damage (getTypeEffectiveness, resolveDamage)
import Pokemonad.Battle.State
  ( BattleAction (..),
    BattlePhase (..),
    BattlePokemon (..),
    BattleState (..),
    Winner (..),
    initBattle,
    flipBattleState,
  )
import Pokemonad.Core.Move (allMoves)
import Pokemonad.Core.Trainer (Trainer (..), allTrainers)
import Pokemonad.Core.Types (HP (..), Level (..), PokemonType (..), Status (..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "game-engine"
    [ aiTests,
      battleTests,
      persistenceTests,
      damageTests
    ]

-- | Estado de combate concreto reutilizable (RED como jugador, BLUE rival).
--   Ambos equipos son no vacíos, así que es un estado válido y completo.
sampleState :: BattleState
sampleState = initBattle (trainerTeam red) blue
  where
    red = allTrainers !! 0
    blue = allTrainers !! 1

isSwitch :: BattleAction -> Bool
isSwitch (ActionSwitch _) = True
isSwitch _ = False

aiTests :: TestTree
aiTests =
  testGroup
    "IA / Aprendizaje por refuerzo"
    [ testCase "el entrenamiento es determinista dado un mismo StdGen" $ do
        let (w1, _, _) = runTrainingEpochs (mkStdGen 42) defaultTrainingHyperParams 3
            (w2, _, _) = runTrainingEpochs (mkStdGen 42) defaultTrainingHyperParams 3
        w1 @?= w2,
      testCase "el entrenamiento efectivamente modifica los pesos" $ do
        let (w, _, _) = runTrainingEpochs (mkStdGen 7) defaultTrainingHyperParams 3
        assertBool "los pesos no deberían quedar en su valor inicial" (w /= defaultQWeights),
      testCase "un paso TD con recompensa positiva acerca Q(s,a) al objetivo" $ do
        let s = sampleState
            a = ActionMove 0
            before = qValue defaultQWeights s a
            w' = tdUpdate defaultTrainingHyperParams defaultQWeights s a 100.0 s
            after = qValue w' s a
        assertBool "Q(s,a) debería aumentar tras un delta positivo" (after > before),
      testCase "chooseActionGreedy elige la acción de mayor Q" $ do
        let s = sampleState
        case chooseActionGreedy defaultQWeights s of
          Nothing -> assertBool "debería existir al menos una acción" False
          Just best ->
            let qs = [qValue defaultQWeights s act | act <- candidateActions s]
             in qValue defaultQWeights s best @?= maximum qs
    ]

battleTests :: TestTree
battleTests =
  testGroup
    "Estado de combate"
    [ testCase "flipBattleState es involutivo (flip . flip = id)" $
        flipBattleState (flipBattleState sampleState) @?= sampleState,
      testCase "candidateActions es vacío cuando la batalla terminó" $
        candidateActions (sampleState {phase = BattleEnded PlayerWon}) @?= [],
      testCase "candidateActions excluye Pokémon debilitados del banco" $ do
        let faintedBench =
              sampleState
                { enemyBench =
                    map (\bp -> bp {battlePokemonStatus = Fainted}) (enemyBench sampleState)
                }
        assertBool
          "no debería ofrecer cambios si todo el banco está debilitado"
          (not (any isSwitch (candidateActions faintedBench)))
    ]

persistenceTests :: TestTree
persistenceTests =
  testGroup
    "Persistencia del checkpoint"
    [ testProperty "decode . encode = id (ley de ida y vuelta)" $ \cp ->
        decodeCheckpoint (encodeCheckpoint cp) == Just cp
    ]

damageTests :: TestTree
damageTests =
  testGroup
    "Daño y efectividad de tipos"
    [ testProperty "la efectividad de tipo siempre está en {0, 0.5, 1, 2}" $ \a b ->
        getTypeEffectiveness a b `elem` [0.0, 0.5, 1.0, 2.0],
      testCase "agua es super efectivo contra fuego (2x)" $
        getTypeEffectiveness Water Fire @?= 2.0,
      testCase "fuego es poco efectivo contra agua (0.5x)" $
        getTypeEffectiveness Fire Water @?= 0.5,
      testProperty "el daño resuelto nunca es negativo" $ \(QC.Positive lvl) (QC.Positive atk) (QC.Positive def) ->
        QC.forAll (elements allMoves) $ \mv ->
          QC.forAll genType $ \dt ->
            unHP (resolveDamage (Level lvl) atk def mv dt) >= 0
    ]

-- Generadores

allTypes :: [PokemonType]
allTypes =
  [ Fire, Water, Grass, Normal, Electric, Bug, Flying, Poison,
    Ground, Rock, Fighting, Psychic, Ghost, Ice, Dragon, Steel, Fairy, Dark
  ]

genType :: Gen PokemonType
genType = elements allTypes

-- | Flotantes "limpios" que sobreviven a un ciclo show/read sin pérdida
--   (evitamos NaN/Infinity, que romperían la igualdad exacta de QWeights).
niceFloat :: Gen Float
niceFloat = (\n -> fromIntegral (n :: Int) / 100) <$> arbitrary

instance QC.Arbitrary PokemonType where
  arbitrary = genType

instance QC.Arbitrary QWeights where
  -- listOf1: el modelo real nunca tiene coeficientes vacíos, y el encoding
  -- de una lista vacía no admite ida y vuelta.
  arbitrary = QWeights <$> niceFloat <*> listOf1 niceFloat

instance QC.Arbitrary AICheckpointData where
  arbitrary = AICheckpointData <$> arbitrary <*> arbitrary <*> niceFloat
