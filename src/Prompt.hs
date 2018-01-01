module Prompt (processPrompt, togglePrompt) where

import           Control.Lens
import           Linear.V2       (V2(V2))
import qualified Helm.Keyboard   as KB
import qualified Data.List       as L

import           Types
import           View
import           Movements

parseBodyName bname =
  case [ (m, n)
       | (m, s1) <- (reads :: ReadS Int) bname
       , ("-", s2) <- lex s1
       , (n, "") <- (reads :: ReadS Int) s2
       ] of
    []           -> Nothing
    [(sid, bid)] -> Just (sid, bid)

getBody :: String -> [SolarSystem] -> Maybe Body
getBody bname ls =
  case parseBodyName bname of
    Nothing         -> Nothing
    Just (sid, bid) -> case ls ^? element (sid - 1) of
      Nothing -> Nothing
      Just s  -> cbodies (sun s) ^? element (bid - 1)

moveFunc model [fle, bod] =
  let fl = fleets model in
  case L.findIndex (\fleet -> fname fleet == fle) fl of
    Nothing  -> model { prompt = Nothing }
    Just fid -> case getBody bod $ systems model of
      Nothing -> model { prompt = Nothing }
      Just b  ->
        let nf = setInterceptBody (fl L.!! fid) b in
        model { prompt = Nothing, fleets = fl & element fid .~ nf }
moveFunc model _            = model { prompt = Nothing }

funcList =
  [
    ("move", moveFunc)
  ]

execCommand :: Model -> String -> Model
execCommand model cmd =
  let l = words cmd in
  case L.find (\(c, f) -> c == head l) funcList of
    Nothing     -> model { prompt = Nothing }
    Just (_, f) -> f model $ tail l

togglePrompt :: Model -> Model
togglePrompt model =
  case prompt model of
    Nothing -> model { prompt = Just "" }
    Just s  -> execCommand model s

processPrompt model key =
  case prompt model of
    Just s  -> case key of
      KB.BackspaceKey   -> if null s then model else model { prompt = Just $ init s }
      key'              -> model { prompt = Just $ s ++ hKeyToChar key' }
    Nothing             -> case key of
      -- Zooming
      KB.KeypadPlusKey  -> model { viewSet = changeZoom   0.1  $ viewSet model }
      KB.KeypadMinusKey -> model { viewSet = changeZoom (-0.1) $ viewSet model }
      -- Panning
      KB.LeftKey        -> model { viewSet = changeOffset (-50) 0  $ viewSet model }
      KB.RightKey       -> model { viewSet = changeOffset 50    0  $ viewSet model }
      KB.DownKey        -> model { viewSet = changeOffset 0    50  $ viewSet model }
      KB.UpKey          -> model { viewSet = changeOffset 0  (-50) $ viewSet model }
      -- Test
      KB.NKey           -> model { fleets = spawnFleet $ fleets model }
      _                 -> model
  where

-- Test
spawnFleet l = Fleet { fpos = V2 150 150, fSysId = 0, speed = 10, fdest = Nothing, fname = "f1", ships = [Ship { hp = 100 }] } : l

hKeyToChar k = case k of
  KB.AKey       -> "a"
  KB.BKey       -> "b"
  KB.CKey       -> "c"
  KB.DKey       -> "d"
  KB.EKey       -> "e"
  KB.FKey       -> "f"
  KB.GKey       -> "g"
  KB.HKey       -> "h"
  KB.IKey       -> "i"
  KB.JKey       -> "j"
  KB.KKey       -> "k"
  KB.LKey       -> "l"
  KB.MKey       -> "m"
  KB.NKey       -> "n"
  KB.OKey       -> "o"
  KB.PKey       -> "p"
  KB.QKey       -> "q"
  KB.RKey       -> "r"
  KB.SKey       -> "s"
  KB.TKey       -> "t"
  KB.UKey       -> "u"
  KB.VKey       -> "v"
  KB.WKey       -> "w"
  KB.XKey       -> "x"
  KB.YKey       -> "y"
  KB.ZKey       -> "z"
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
