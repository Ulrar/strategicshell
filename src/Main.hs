import           Helm
import           Helm.Engine     (windowSize)
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Cmd        as Cmd
import qualified Helm.Keyboard   as KB
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Time       as Time
import qualified Helm.Sub        as Sub
import qualified Helm.Window     as Win

import           Linear.V2       (V2(V2))
import qualified Data.List       as L

import           View
import           Types
import           Movements
import           Generation

initial size offset zoom = (Model (genUniverse) 0 size offset zoom, Cmd.none)

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
