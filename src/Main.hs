import           Helm
import           Helm.Engine     (windowSize)
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Cmd        as Cmd
import qualified Helm.Keyboard   as KB
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Time       as Time
import qualified Helm.Sub        as Sub
import qualified Helm.Window     as Win

import           Data.Angle
import           Linear.Vector
import           Linear.V2       (V2(V2))
import           Linear.Metric   (distance, quadrance, normalize)
import qualified Data.List       as L

import           View
import           Types
import           Generation

calcObjectCoord r t (V2 px py) = V2 (px + (sine $ Degrees t) * r) (py + (cosine $ Degrees t) * r)

initial size offset zoom = (Model (genUniverse) 0 size offset zoom, Cmd.none)

makeOrbit :: V2 Double -> Body -> Body
-- Don't do anything for the sun
makeOrbit (V2 0 0) (Body (V2 0 0) 0 0 c r b) = Body (V2 0 0) 0 0 c r $ L.map (makeOrbit (V2 0 0)) b
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
---
---
---

changeOffset ix iy (V2 x y) = V2 (x + ix) (y + iy)

-- Test
spawnFleet (h:t) = (h { fleets = [Fleet { fpos = V2 150 150, speed = 10, fdest = Nothing, ships = [Ship { hp = 100, blah = "blah" }] }] }):t
moveFleet (h:t) = (h { fleets = [setInterceptBody (head $ fleets h) ((cbodies $ sun h) L.!! 2)] }):t

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) = (model { screenSize = size }, Cmd.none)
update model (Tick t)             = (model { systems = L.map (\s -> SolarSystem (makeOrbit (V2 0 0) $ sun s) $ L.map makeFleetMove $ fleets s) $ systems model }, Cmd.none)
-- Zooming
update model (KeyPressed KB.KeypadPlusKey)  = (model { viewZoom = (viewZoom model) + 0.1 }, Cmd.none)
update model (KeyPressed KB.KeypadMinusKey) = (model { viewZoom = (viewZoom model) - 0.1 }, Cmd.none)
-- Panning
update model (KeyPressed KB.LeftKey)        = (model { viewOffset = changeOffset (-50) 0  $ viewOffset model }, Cmd.none)
update model (KeyPressed KB.RightKey)       = (model { viewOffset = changeOffset 50    0  $ viewOffset model }, Cmd.none)
update model (KeyPressed KB.DownKey)        = (model { viewOffset = changeOffset 0    50  $ viewOffset model }, Cmd.none)
update model (KeyPressed KB.UpKey)          = (model { viewOffset = changeOffset 0  (-50) $ viewOffset model }, Cmd.none)
-- Test
update model (KeyPressed KB.NKey)           = (model { systems = spawnFleet $ systems model }, Cmd.none)
update model (KeyPressed KB.PKey)           = (model { systems = moveFleet $ systems model }, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [Time.every (Time.millisecond * 70) Tick, Win.resizes WindowResized, KB.presses (\b -> KeyPressed b)]

main :: IO ()
main = do
  engine <- SDL.startup
  size <- windowSize engine

  run engine GameConfig
    { initialFn       = initial size (V2 0 0) 1
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
