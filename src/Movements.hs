module Movements
       (
         makeOrbit
       , makeFleetMove
       , setInterceptBody
       ) where

import           Data.Angle
import           Linear.Vector
import           Linear.V2       (V2(V2))
import           Linear.Metric   (distance, quadrance, normalize)
import qualified Data.List       as L

import           Types

calcObjectCoord r t (V2 px py) = V2 (px + (sine $ Degrees t) * r) (py + (cosine $ Degrees t) * r)

makeOrbit :: V2 Double -> Body -> Body
-- Don't do anything for the sun
makeOrbit (V2 0 0) (Body (V2 0 0) n 0 0 c r b) = Body (V2 0 0) n 0 0 c r $ L.map (makeOrbit (V2 0 0)) b
-- Orbit
makeOrbit pc b =
  let t = curOr b in
  let d = dfp b in
  let nt = (if t > 360 then t - 360 else t) + (360 / d) in
  let coord = calcObjectCoord d nt pc in
  b { bpos = coord, curOr = nt, cbodies = L.map (makeOrbit coord) $ cbodies b }

makeFleetMove f =
  case fdest f of
    Nothing   -> f
    Just dest ->
      if (distance (fpos f) dest) < (fromIntegral $ speed f)
       then
         f { fpos = dest, fdest = Nothing }
       else
         let n = normalize $ dest ^-^ (fpos f) in
         let mov = n ^* (fromIntegral $ speed f) in
         f { fpos = (fpos f) ^+^ mov }



---
--- Thanks to DMGregory's answer on
--- https://gamedev.stackexchange.com/questions/75015/how-can-i-intercept-object-with-a-circular-motion
---
positionAt b t =
  let (V2 x y) = bpos b in
  let (Radians orbitalSpeed) = radians $ Degrees (360 / dfp b) in
  let angle = (atan2 x y) + t * orbitalSpeed in
  (V2 (sin angle) (cos angle) ^* (dfp b))

search tMax f b be bc tCur =
  if tCur >= tMax
    then
      bc
    else
      let coord = positionAt b tCur in
      let sp = fromIntegral $ speed f in
      let err = (quadrance (coord ^-^ (fpos f))) / (sp * sp) - (tCur * tCur) in
      if (abs err) < be
        then
          search tMax f b (abs err) coord (tCur + 1)
        else
          search tMax f b be bc (tCur + 1)

setInterceptBody f b =
  let shipRadius = distance (fpos f) (V2 0 0) in
  let shortestDist = abs $ shipRadius - (dfp b) in
  let sp = fromIntegral $ speed f in
  let tMin = shortestDist / sp in
  let tMax = (dfp b) * 2 / sp + (if shipRadius > (dfp b) then tMin else -tMin) in
  let dest = search tMax f b 99999 (V2 0 0) tMin in
  f { fdest = Just dest }

