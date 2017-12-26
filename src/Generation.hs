module Generation (genUniverse) where

import           System.Random

import           Types

genBody :: RandomGen g => Int -> g -> ([Body], g)
genBody 0 g = ([], g)
genBody n g =
  let (curOr, ng) = randomR (0, 360) g in
  let (r, ng') = randomR (5, 30) ng in
  let (nb, fg) = genBody (n - 1) ng' in
  (nb ++ [Body 0 0 (fromIntegral $ n * 200) curOr r []], fg)

genSolarSystem :: RandomGen g => Int -> g -> ([SolarSystem], g)
genSolarSystem 0 g = ([], g)
genSolarSystem n g =
  let (nb, ng) = randomR (1, 10) g in
  let (l, ng') = genBody nb ng in
  let (ns, fg) = genSolarSystem (n - 1) ng' in
  (ns ++ [SolarSystem l []], fg)

genUniverse = let (ss, g) = genSolarSystem 100 $ mkStdGen 42 in ss
