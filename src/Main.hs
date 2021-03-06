import           Helm
import           Helm.Engine          (windowSize)
import           Helm.Engine.SDL      (SDLEngine)
import qualified Helm.Cmd             as Cmd
import qualified Helm.Keyboard        as KB
import qualified Helm.Engine.SDL      as SDL
import qualified Helm.Time            as Time
import qualified Helm.Sub             as Sub
import qualified Helm.Window          as Win

import           Linear.V2            (V2(V2))
import qualified Data.List            as L
import qualified Data.HashMap.Strict  as Map

import           View
import           Types
import           Prompt
import           Movements
import           Generation

initial viewS =
  let (uni, nMap) = genUniverse Map.empty in
  (Model uni nMap Map.empty viewS False (Shell Nothing []), Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) =
  ( model { viewSet = changeScreenSize size $ viewSet model }
  , Cmd.none
  )
update model (Tick t)             = let syst = systems model in
  ( model
      { systems = L.map (\s -> SolarSystem (makeOrbit (V2 0 0) $ sun s)) syst
      , fleets = Map.map makeFleetMove $ fleets model
      }
  , Cmd.none
  )
-- Prompt
update model (KeyPressed KB.ReturnKey) = (togglePrompt model, Cmd.none)
update model (KeyPressed k)            = (processKey model k, Cmd.none)
-- Shift Key
update model (KeyUp   KB.RightShiftKey) = (model { shiftKey = False }, Cmd.none)
update model (KeyDown KB.RightShiftKey) = (model { shiftKey = True  }, Cmd.none)
update model (KeyUp   _)                = (model, Cmd.none)
update model (KeyDown _)                = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Time.every (Time.millisecond * 70) Tick
  , Win.resizes WindowResized
  , KB.presses KeyPressed
  , KB.ups KeyUp
  , KB.downs KeyDown
  ]

main :: IO ()
main = do
  engine <- SDL.startup
  size <- windowSize engine
  let viewS = ViewSettings 0 size (V2 0 0) 1

  run engine defaultConfig GameLifecycle
    { initialFn       = initial viewS
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
