import Helm
import Helm.Engine (windowSize)
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D
import Helm.Color

import           Control.Lens.Operators
import qualified Control.Lens as LS
import Linear.V2 (V2(V2))
import Data.Angle

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

data Model = Model
  {
    bodies  :: [Body],
    size    :: V2 Int,
    offset  :: V2 Double,
    zoom    :: Double
  }

calcObjectCoord r t px py = (px + (sine $ Degrees t) * r, py + (cosine $ Degrees t) * r)

genBody r dfp c b = Body 0 0 dfp c r b

initial :: V2 Int -> (Model, Cmd SDLEngine Action)
initial size = (Model [genBody 10 100 50 [], genBody 15 450 0 [genBody 3 50 0 []], genBody 5 210 290 []] size (V2 0 0) 1, Cmd.none)

makeOrbit px py (Body _ _ dfp t r b) = let nt = (if t > 360 then t - 360 else t) + (360 / dfp) in let (x, y) = calcObjectCoord dfp nt px py in Body x y dfp nt r $ L.map (makeOrbit x y) b

changeOffset ix iy (V2 x y) = V2 (x + ix) (y + iy)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) = (model { size = size }, Cmd.none)
update model (Tick t)             = (model { bodies = L.map (makeOrbit 0 0) $ bodies model }, Cmd.none)
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
view (Model lp ss offset zoom) = Graphics2D $ collage $ [move (inGameToScreenCoord 0 0 offset ss zoom) $ filled (rgb 1 1 1) $ circle (20 * zoom)] ++ [renderBodies zoom offset ss lp]

main :: IO ()
main = do
  engine <- SDL.startup
  size <- windowSize engine

  run engine GameConfig
    { initialFn       = initial size
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
