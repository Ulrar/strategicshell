module           Prompt
                 ( processKey
                 , togglePrompt
                 )
where

import           Control.Lens
import           Linear.V2           (V2(V2))
import           Data.Char           (toUpper)
import qualified Helm.Keyboard       as KB
import qualified Data.List           as L
import qualified Data.HashMap.Strict as Map

import           Types
import           View
import           Movements

getPlanet :: Int -> Int -> [SolarSystem] -> Maybe Body
getPlanet sid bid ls =
  case ls ^? element sid of
    Nothing -> Nothing
    Just s  -> cbodies (sun s) ^? element bid

getBody :: [Int] -> [Body] -> Maybe Body
getBody [] _     = Nothing
getBody [n] bl   = bl ^? element n
getBody (n:t) bl = case bl ^? element n of
  Nothing -> Nothing
  Just b  -> getBody t $ cbodies b

lookupBody :: [Int] -> [SolarSystem] -> Maybe Body
lookupBody [] _     = Nothing
lookupBody [n] sl   = case sl ^? element n of
  Nothing -> Nothing
  Just s  -> Just $ sun s
lookupBody (n:t) sl = case sl ^? element n of
  Nothing -> Nothing
  Just s  -> getBody t (cbodies $ sun s)

updateBodyInSystem :: (Body -> Body) -> [Int] -> [Body] -> [Body]
updateBodyInSystem _ [] bl    = bl
updateBodyInSystem f [n] bl   = case bl ^? element n of
  Nothing -> bl
  Just b  -> bl & element n .~ f b
updateBodyInSystem f (n:t) bl = case bl ^? element n of
  Nothing -> bl
  Just b  ->
    bl & element n .~ b { cbodies = updateBodyInSystem f t (cbodies b) }

updateBody :: (Body -> Body) -> [Int] -> [SolarSystem] -> [SolarSystem]
updateBody _ [] sl    = sl
updateBody f [n] sl   = case sl ^? element n of
  Nothing -> sl
  Just s  -> sl & element n .~ s { sun = f $ sun s }
updateBody f (n:t) sl = case sl ^? element n of
  Nothing -> sl
  Just s  -> let b = sun s in
    sl & element n .~
      s { sun = b { cbodies = updateBodyInSystem f t $ cbodies b } }

resetPrompt model t =
  let s = shell model in
  let h = history s in
  if null t
    then
      model { shell = s { prompt = Nothing } }
    else
      model
        {
          shell = s { prompt  = Nothing
                    , history = case prompt s of
                                  Nothing  -> t : h
                                  Just cmd -> t : ("> " ++ cmd) : h
                    }
        }

moveFunc :: Model -> [String] -> Model
moveFunc model [fle, bod] =
  let fm = fleets model in
  let s = shell model in
  case Map.lookup fle fm of
    Nothing    -> resetPrompt model $ "No fleet by that name : " ++ fle
    Just fleet -> case Map.lookup bod $ bodyNames model of
      Nothing      -> resetPrompt model $ "No body by that name : " ++ bod
      Just bodyIds -> case lookupBody bodyIds $ systems model of
        Nothing -> resetPrompt model
                   $ "Something went wrong looking up body : " ++ bod
        Just b  ->
          let nf = setInterceptBody fleet b in
          resetPrompt (model {fleets = Map.insert fle nf fm })
          $ "Moving " ++ fle ++ " to " ++ bod
moveFunc model _            = 
  let s = shell model in
  resetPrompt model "usage : move <fleet name> <body name>"

renameBodyFunc model [curName, newName] =
  case Map.lookup curName $ bodyNames model of
    Nothing -> resetPrompt model $ "No body by that name : " ++ curName
    Just x  ->
      let nm = Map.insert newName x $ Map.delete curName $ bodyNames model in
      let sl = updateBody (\b -> b { bname = newName }) x $ systems model in
      resetPrompt (model { bodyNames = nm, systems = sl })
      $ "Renamed" ++ curName ++ " to " ++ newName
renameBodyFunc model _                  =
  let s = shell model in
  resetPrompt model "usage : renameBody <old name> <new name>"

funcList :: [(String, Model -> [String] -> Model)]
funcList =
  [ ("move"      , moveFunc)
  , ("renameBody", renameBodyFunc)
  ]

execCommand :: Model -> String -> Model
execCommand model cmd =
  let l = words cmd in
  let s = shell model in
  if null l
    then
      resetPrompt model ""
    else
      case L.find (\(c, f) -> c == head l) funcList of
        Nothing     -> resetPrompt model $ "Unknown command : " ++ head l
        Just (_, f) -> f model $ tail l

togglePrompt :: Model -> Model
togglePrompt model =
  let s = shell model in
  case prompt s of
    Nothing -> model { shell = s { prompt = Just "" } }
    Just s  -> execCommand model s

processPrompt model cmd key =
  let s = shell model in
  case key of
    KB.BackspaceKey -> if null cmd
      then model
      else model { shell = s { prompt = Just $ init cmd } }
    key'            ->
      model
        {
          shell = s
            {
              prompt = Just $ cmd ++ hKeyToChar (shiftKey model) key'
            }
        }

processKey model key =
  let s = shell model in
  let viewS = viewSet model in
  case prompt s of
    Just cmd -> processPrompt model cmd key
    Nothing  -> case key of
      -- Zooming
      KB.KeypadPlusKey  -> model { viewSet = changeZoom   0.1  viewS }
      KB.KeypadMinusKey -> model { viewSet = changeZoom (-0.1) viewS }
      -- Panning
      KB.LeftKey        -> model { viewSet = changeOffset (-50) 0  viewS }
      KB.RightKey       -> model { viewSet = changeOffset 50    0  viewS }
      KB.DownKey        -> model { viewSet = changeOffset 0    50  viewS }
      KB.UpKey          -> model { viewSet = changeOffset 0  (-50) viewS }
      -- Test
      KB.NKey           -> model { fleets = spawnFleet $ fleets model }
      _                 -> model
  where

-- Test
spawnFleet = Map.insert "f1"
  Fleet
    { fpos = V2 150 150
    , fSysId = 0
    , speed = 10
    , fdest = Nothing
    , fname = "f1"
    , ships = [Ship { hp = 100 }]
    }

keyCodeToChar k = case k of
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
  KB.Number0Key -> "0"
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
  _             -> ""

hKeyToChar sk k =
  let key = keyCodeToChar k in
  if sk then L.map toUpper key else key
