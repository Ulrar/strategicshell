import           Helm
import           Data.Maybe      (isJust)
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
import           Prompt
import           Movements
import           Generation

initial size offset zoom = (Model (genUniverse) [] 0 size offset zoom Nothing, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) = (model { screenSize = size }, Cmd.none)
update model (Tick t)             = (model { systems = L.map (\s -> SolarSystem (makeOrbit (V2 0 0) $ sun s)) $ systems model, fleets = L.map makeFleetMove $ fleets model }, Cmd.none)
-- Prompt
update model (KeyPressed KB.ReturnKey) = (if isJust $ prompt model then model { prompt = Nothing } else model { prompt = Just "" }, Cmd.none)
update model (KeyPressed k)            = (processPrompt model k, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [Time.every (Time.millisecond * 70) Tick, Win.resizes WindowResized, KB.presses (\b -> KeyPressed b)]

main :: IO ()
main = do
  engine <- SDL.startup
  size <- windowSize engine

  run engine defaultConfig GameLifecycle
    { initialFn       = initial size (V2 0 0) 1
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
