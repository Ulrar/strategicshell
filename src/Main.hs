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
import           Linear.V2       (V2(V2))
import qualified Data.List       as L

import           View
import           Types
import           Generation

calcObjectCoord r t px py = (px + (sine $ Degrees t) * r, py + (cosine $ Degrees t) * r)

initial size offset zoom = (Model (genUniverse) 0 size offset zoom, Cmd.none)

makeOrbit 0 0 (Body 0 0 0 0 c r b) = Body 0 0 0 0 c r $ L.map (makeOrbit 0 0) b
makeOrbit px py (Body _ _ dfp t c r b) =
  let nt = (if t > 360 then t - 360 else t) + (360 / dfp) in
  let (x, y) = calcObjectCoord dfp nt px py in
  Body x y dfp nt c r $ L.map (makeOrbit x y) b

changeOffset ix iy (V2 x y) = V2 (x + ix) (y + iy)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) = (model { screenSize = size }, Cmd.none)
update model (Tick t)             = (model { systems = L.map (\s -> SolarSystem (makeOrbit 0 0 $ sun s) []) $ systems model }, Cmd.none)
-- Zooming
update model (KeyPressed KB.KeypadPlusKey)  = (model { viewZoom = (viewZoom model) + 0.1 }, Cmd.none)
update model (KeyPressed KB.KeypadMinusKey) = (model { viewZoom = (viewZoom model) - 0.1 }, Cmd.none)
-- Panning
update model (KeyPressed KB.LeftKey)        = (model { viewOffset = changeOffset (-50) 0  $ viewOffset model }, Cmd.none)
update model (KeyPressed KB.RightKey)       = (model { viewOffset = changeOffset 50    0  $ viewOffset model }, Cmd.none)
update model (KeyPressed KB.DownKey)        = (model { viewOffset = changeOffset 0    50  $ viewOffset model }, Cmd.none)
update model (KeyPressed KB.UpKey)          = (model { viewOffset = changeOffset 0  (-50) $ viewOffset model }, Cmd.none)

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