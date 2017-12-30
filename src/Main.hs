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
import           Movements
import           Generation

hKeyToChar k = case k of
  KB.AKey -> "a"
  KB.BKey -> "b"
  KB.CKey -> "c"
  KB.DKey -> "d"
  KB.EKey -> "e"
  KB.FKey -> "f"
  KB.GKey -> "g"
  KB.HKey -> "h"
  KB.IKey -> "i"
  KB.JKey -> "j"
  KB.KKey -> "k"
  KB.LKey -> "l"
  KB.MKey -> "m"
  KB.NKey -> "n"
  KB.OKey -> "o"
  KB.PKey -> "p"
  KB.QKey -> "q"
  KB.RKey -> "r"
  KB.SKey -> "s"
  KB.TKey -> "t"
  KB.UKey -> "u"
  KB.VKey -> "v"
  KB.WKey -> "w"
  KB.XKey -> "x"
  KB.YKey -> "y"
  KB.ZKey -> "z"

initial size offset zoom = (Model (genUniverse) 0 size offset zoom Nothing, Cmd.none)

changeOffset ix iy (V2 x y) = V2 (x + ix) (y + iy)

-- Test
spawnFleet (h:t) = (h { fleets = [Fleet { fpos = V2 150 150, speed = 10, fdest = Nothing, ships = [Ship { hp = 100, blah = "blah" }] }] }):t
moveFleet (h:t) = (h { fleets = [setInterceptBody (head $ fleets h) ((cbodies $ sun h) L.!! 2)] }):t

-- Process keystrokes
processPrompt model key =
  case prompt model of
    Just s  -> model { prompt = Just $ s ++ hKeyToChar key }
    Nothing -> case key of
      -- Zooming
      KB.KeypadPlusKey  -> model { viewZoom = (viewZoom model) + 0.1 }
      KB.KeypadMinusKey -> model { viewZoom = (viewZoom model) - 0.1 }
      -- Panning
      KB.LeftKey        -> model { viewOffset = changeOffset (-50) 0  $ viewOffset model }
      KB.RightKey       -> model { viewOffset = changeOffset 50    0  $ viewOffset model }
      KB.DownKey        -> model { viewOffset = changeOffset 0    50  $ viewOffset model }
      KB.UpKey          -> model { viewOffset = changeOffset 0  (-50) $ viewOffset model }
      -- Test
      KB.NKey           -> model { systems = spawnFleet $ systems model }
      KB.PKey           -> model { systems = moveFleet $ systems model }
      _                 -> model

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (WindowResized size) = (model { screenSize = size }, Cmd.none)
update model (Tick t)             = (model { systems = L.map (\s -> SolarSystem (makeOrbit (V2 0 0) $ sun s) $ L.map makeFleetMove $ fleets s) $ systems model }, Cmd.none)
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
