import Helm
import Helm.Engine (windowSize)
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D
import Helm.Color

import           Control.Lens.Operators
import qualified Control.Lens as LS
import Linear.V2 (V2(V2))
import Data.Angle
import System.Random

import qualified Data.List as L
import qualified Helm.Cmd as Cmd
import qualified Helm.Keyboard as KB
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Time as Time
import qualified Helm.Sub as Sub
import qualified Helm.Window as Win

data Action = Tick Double | WindowResized (V2 Int) | KeyPressed KB.Key

data Body = Body
  {
    x       :: Double,
    y       :: Double,
    dfp     :: Double,
    curOr   :: Double,
    radius  :: Double,
    cbodies :: [Body]
  }

data Ship = Ship
  {
    hp              :: Int,
    blah            :: String
  }

data Fleet = Fleet
  {
    fx               :: Double,
    fy               :: Double,
    ships            :: [Ship]
  }

data SolarSystem = SolarSystem
  {
    bodies          :: [Body],
    fleets          :: [Fleet]
  }

data Model = Model
  {
    systems     :: [SolarSystem],
    dispSysId   :: Int,
    size        :: V2 Int,
    offset      :: V2 Double,
    zoom        :: Double
  }

calcObjectCoord r t px py = (px + (sine $ Degrees t) * r, py + (cosine $ Degrees t) * r)

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

initial size offset zoom = (Model (genUniverse) 0 size offset zoom, Cmd.none)

makeOrbit px py (Body _ _ dfp t r b) = let nt = (if t > 360 then t - 360 else t) + (360 / dfp) in let (x, y) = calcObjectCoord dfp nt px py in Body x y dfp nt r $ L.map (makeOrbit x y) b

changeOffset ix iy (V2 x y) = V2 (x + ix) (y + iy)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) = (model { size = size }, Cmd.none)
update model (Tick t)             = (model { systems = L.map (\s -> SolarSystem (L.map (makeOrbit 0 0) $ bodies s) []) $ systems model }, Cmd.none)
-- Zooming
update model (KeyPressed KB.KeypadPlusKey)  = (model { zoom = (zoom model) + 0.1 }, Cmd.none)
update model (KeyPressed KB.KeypadMinusKey) = (model { zoom = (zoom model) - 0.1 }, Cmd.none)
-- Panning
update model (KeyPressed KB.LeftKey)        = (model { offset = changeOffset (-50) 0  $ offset model }, Cmd.none)
update model (KeyPressed KB.RightKey)       = (model { offset = changeOffset 50    0  $ offset model }, Cmd.none)
update model (KeyPressed KB.DownKey)        = (model { offset = changeOffset 0    50  $ offset model }, Cmd.none)
update model (KeyPressed KB.UpKey)          = (model { offset = changeOffset 0  (-50) $ offset model }, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [Time.every (Time.millisecond * 70) Tick, Win.resizes WindowResized, KB.presses (\b -> KeyPressed b)]

inGameToScreenCoord :: Double -> Double -> V2 Double -> V2 Int -> Double -> V2 Double
inGameToScreenCoord x y (V2 offsetx offsety) (V2 screenx screeny) zoom = V2 (zoom * (x - offsetx) + (fromIntegral screenx / 2)) (zoom * (y - offsety) + (fromIntegral screeny / 2))

renderBody zoom offset ss (Body x y _ _ r b) = move (inGameToScreenCoord x y offset ss zoom) $ filled (rgb 1 0 0) $ circle (r * zoom)

renderBodies zoom offset ss b = toForm $ collage $ (L.map (renderBody zoom offset ss) b) ++ (L.map ((renderBodies zoom offset ss) . cbodies) b)

view :: Model -> Graphics SDLEngine
view (Model lsolsys dsi ss offset zoom) = Graphics2D $ collage $ [move (inGameToScreenCoord 0 0 offset ss zoom) $ filled (rgb 1 1 1) $ circle (20 * zoom)] ++
  (if (L.length lsolsys) - 1 < dsi then [] else [renderBodies zoom offset ss (bodies $ lsolsys L.!! dsi)])

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
