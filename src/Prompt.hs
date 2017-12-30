module Prompt (processPrompt) where

import           Linear.V2       (V2(V2))
import qualified Helm.Keyboard   as KB
import qualified Data.List       as L

import           Types
import           View
import           Movements

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
      KB.NKey           -> model { fleets = spawnFleet $ fleets model }
      KB.PKey           -> model { fleets = moveFleet (fleets model) $ systems model }
      _                 -> model
  where

-- Test
spawnFleet l = [Fleet { fpos = V2 150 150, fSysId = 0, speed = 10, fdest = Nothing, fname = "F1", ships = [Ship { hp = 100 }] }] ++ l
moveFleet l s = [setInterceptBody (head l) ((cbodies $ sun $ (s L.!! 0)) L.!! 2)]

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
