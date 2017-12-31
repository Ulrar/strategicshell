module Generation (genUniverse) where

import           Helm.Color
import           System.Random
import           Linear.V2        (V2(V2))

import           Types

genMoon :: RandomGen g => Int -> Int -> Int -> Double -> g -> ([Body], g)
genMoon _ _ 0 _ g = ([], g)
genMoon sn bn n pr g =
  let (curOr, ng) = randomR (0, 360) g in
  let (r, ng') = randomR (1, 5) ng in
  let (nm, fg) = genMoon sn bn (n - 1) pr ng' in
  (nm ++ [Body (V2 0 0) (show sn ++ "-" ++ show bn ++ "-" ++ show n) ((fromIntegral n * 10) + pr) curOr (rgb 1 0 0) r []], fg)

genBody :: RandomGen g => Int -> Int -> g -> ([Body], g)
genBody _ 0 g = ([], g)
genBody sn n g =
  let (curOr, ng) = randomR (0, 360) g in
  let (r, ng') = randomR (5, 30) ng in
  let (nm, ng'') = randomR (0, 3) ng' in
  let (m, ng''') = genMoon sn n nm r g in
  let (nb, fg) = genBody sn (n - 1) ng'' in
  (nb ++ [Body (V2 0 0) (show sn ++ "-" ++ show n) (fromIntegral $ n * 200) curOr (rgb 0 1 0) r m], fg)

genSolarSystem :: RandomGen g => Int -> g -> ([SolarSystem], g)
genSolarSystem 0 g = ([], g)
genSolarSystem n g =
  let (nb, ng) = randomR (1, 10) g in
  let (l, ng') = genBody n nb ng in
  let (ns, fg) = genSolarSystem (n - 1) ng' in
  (ns ++ [SolarSystem (Body (V2 0 0) ("Sun-" ++ show n) 0 0 (rgb 1 1 1) 100 l)], fg)

genUniverse = let (ss, g) = genSolarSystem 100 $ mkStdGen 42 in ss
