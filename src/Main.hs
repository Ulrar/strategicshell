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

calcObjectCoord r t px py = (px + (sine $ Degrees t) * r, py + (cosine $ Degrees t) * r)

initial size offset zoom = (Model (genUniverse) 0 size offset zoom, Cmd.none)

makeOrbit :: Double -> Double -> Body -> Body
-- Don't do anything for the sun
makeOrbit 0 0 (Body 0 0 0 0 c r b) = Body 0 0 0 0 c r $ L.map (makeOrbit 0 0) b
-- Orbit
makeOrbit px py b =
  let t = curOr b in
  let d = dfp b in
  let nt = (if t > 360 then t - 360 else t) + (360 / d) in
  let (x, y) = calcObjectCoord d nt px py in
  b { x = x, y = y, curOr = nt, cbodies = L.map (makeOrbit x y) $ cbodies b }

makeFleetMove f =
  let cur = V2 (fx f) (fy f) in
  let dest = V2 (fdestx f) (fdesty f) in
  if (distance cur dest) < (fromIntegral $ speed f)
   then
     f { fx = fdestx f, fy = fdesty f }
   else
     let n = normalize $ dest ^-^ cur in
     let (V2 x y) = n ^* (fromIntegral $ speed f) in
     f { fx = (fx f + x), fy = (fy f + y) }

---
--- Thanks to DMGregory's answer on
--- https://gamedev.stackexchange.com/questions/75015/how-can-i-intercept-object-with-a-circular-motion
---
positionAt b t =
  let (Radians orbitalSpeed) = radians $ Degrees (360 / dfp b) in
  let angle = (atan2 (x b) (y b)) + t * orbitalSpeed in
  (V2 (sin angle) (cos angle) ^* (dfp b))

search tMax f b be bx by tCur =
  if tCur >= tMax
    then
      (bx, by)
    else
      let (V2 x y) = positionAt b tCur in
      let sp = fromIntegral $ speed f in
      let err = (quadrance ((V2 x y) ^-^ (V2 (fx f) (fy f)))) / (sp * sp) - (tCur * tCur) in
      if (abs err) < be
        then
          search tMax f b (abs err) x y (tCur + 1)
        else
          search tMax f b be bx by (tCur + 1)

setInterceptBody f b =
  let shipRadius = distance (V2 (fx f) (fy f)) (V2 0 0) in
  let shortestDist = abs $ shipRadius - (dfp b) in
  let sp = fromIntegral $ speed f in
  let tMin = shortestDist / sp in
  let tMax = (dfp b) * 2 / sp + (if shipRadius > (dfp b) then tMin else -tMin) in
  let (x, y) = search tMax f b 99999 0 0 tMin in
  f { fdestx = x, fdesty = y }
---
---
---

changeOffset ix iy (V2 x y) = V2 (x + ix) (y + iy)

-- Test
spawnFleet (h:t) = (h { fleets = [Fleet { fx = 150, fy = 150, speed = 10, fdestx = 900, fdesty = 900, ships = [Ship { hp = 100, blah = "blah" }] }] }):t
moveFleet (h:t) = (h { fleets = [setInterceptBody (head $ fleets h) ((cbodies $ sun h) L.!! 2)] }):t

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) = (model { screenSize = size }, Cmd.none)
update model (Tick t)             = (model { systems = L.map (\s -> SolarSystem (makeOrbit 0 0 $ sun s) $ L.map makeFleetMove $ fleets s) $ systems model }, Cmd.none)
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
