module           Generation
                 (genUniverse)
where

import           Helm.Color
import           System.Random
import           Linear.V2            (V2(V2))
import qualified Data.HashMap.Strict  as Map

import           Types

genMoon :: RandomGen g => Map.HashMap String [Int] -> Int -> Int -> Int -> Double -> g -> ([Body], g, Map.HashMap String [Int])
genMoon m _ _ 0 _ g = ([], g, m)
genMoon nameMap sn bn n pr g =
  let (curOr, ng) = randomR (0, 360) g in
  let (r, ng') = randomR (1, 5) ng in
  let (nm, fg, nameMap') = genMoon nameMap sn bn (n - 1) pr ng' in
  let name = show sn ++ "-" ++ show bn ++ "-" ++ show n in
  (nm ++ [Body (V2 0 0) name ((fromIntegral n * 10) + pr) curOr (rgb 1 0 0) r []], fg, Map.insert name [sn - 1, bn - 1, n - 1] nameMap')

genBody :: RandomGen g => Map.HashMap String [Int] -> Int -> Int -> g -> ([Body], g, Map.HashMap String [Int])
genBody m _ 0 g = ([], g, m)
genBody nameMap sn n g =
  let (curOr, ng) = randomR (0, 360) g in
  let (r, ng') = randomR (5, 30) ng in
  let (nm, ng'') = randomR (0, 3) ng' in
  let (m, ng''', nameMap') = genMoon nameMap sn n nm r g in
  let (nb, fg, nameMap'') = genBody nameMap' sn (n - 1) ng'' in
  let name = show sn ++ "-" ++ show n in
  (nb ++ [Body (V2 0 0) name (fromIntegral $ n * 200) curOr (rgb 0 1 0) r m], fg, Map.insert name [sn - 1, n - 1] nameMap'')

genSolarSystem :: RandomGen g => Map.HashMap String [Int] -> Int -> g -> ([SolarSystem], g, Map.HashMap String [Int])
genSolarSystem m 0 g = ([], g, m)
genSolarSystem nameMap n g =
  let (nb, ng) = randomR (1, 10) g in
  let (l, ng', nameMap') = genBody nameMap n nb ng in
  let (ns, fg, nameMap'') = genSolarSystem nameMap' (n - 1) ng' in
  let name = "Sun-" ++ show n in
  (ns ++ [SolarSystem (Body (V2 0 0) name 0 0 (rgb 1 1 1) 100 l)], fg, Map.insert name [n - 1] nameMap'')

genUniverse nameMap = let (ss, g, m) = genSolarSystem nameMap 100 $ mkStdGen 42 in (ss, m)
