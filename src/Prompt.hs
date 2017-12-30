module Prompt (processPrompt, togglePrompt) where

import           Linear.V2       (V2(V2))
import qualified Helm.Keyboard   as KB
import qualified Data.List       as L

import           Types
import           View
import           Movements

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (h:t)
  | i == 0       = t
  | otherwise    = h : deleteN (i - 1) t

execCommand model cmd =
  let l = words cmd in
  case head l of
    "move" ->
      let fid'= L.findIndex (\fleet -> fname fleet == l L.!! 1) $ fleets model in
      case fid' of
        Nothing  -> model { prompt = Nothing }
        Just fid ->
          let sid' = takeWhile (\c -> c /= '-') $ l L.!! 2 in
          let bid' = drop 1 $ dropWhile (\c -> c /= '-') $ l L.!! 2 in
          let sid = (read sid') - 1 in
          let bid = (read bid') - 1 in
          let b = (cbodies $ sun $ (systems model) L.!! sid) L.!! bid in
          model { prompt = Nothing, fleets = [setInterceptBody (fleets model L.!! fid) b] ++ (deleteN fid $ fleets model) }
    _      -> model { prompt = Nothing }

togglePrompt model =
  case prompt model of
    Nothing -> model { prompt = Just "" }
    Just s  -> execCommand model s

processPrompt model key =
  case prompt model of
    Just s  -> case key of
      KB.BackspaceKey -> if null s then model else model { prompt = Just $ init s }
      key'            -> model { prompt = Just $ s ++ hKeyToChar key' }
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
spawnFleet l = [Fleet { fpos = V2 150 150, fSysId = 0, speed = 10, fdest = Nothing, fname = "f1", ships = [Ship { hp = 100 }] }] ++ l
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
  KB.Number1Key -> "1"
  KB.Number2Key -> "2"
  KB.Number3Key -> "3"
  KB.Number4Key -> "4"
  KB.Number5Key -> "5"
  KB.Number6Key -> "6"
  KB.Number7Key -> "7"
  KB.Number8Key -> "8"
  KB.Number9Key -> "9"
  KB.SpaceKey   -> " "
  KB.MinusKey   -> "-"
