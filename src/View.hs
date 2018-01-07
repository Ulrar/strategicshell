module           View
                 ( view
                 , changeOffset
                 , changeScreenSize
                 , changeZoom
                 )
where

import           Helm
import           Helm.Color
import           Helm.Graphics2D
import           Linear.V2            (V2(V2))
import           Helm.Engine.SDL      (SDLEngine)
import qualified Helm.Graphics2D.Text as HT
import qualified Data.List            as L
import qualified Data.HashMap.Strict  as Map

import           Types

--
-- Update ViewSettings
--

-- Update the offset by adding the arguments to the current values
changeOffset ix iy viewS =
  let (V2 x y) = viewOffset viewS in
  viewS { viewOffset = V2 (x + ix) (y + iy) }

-- Update the screen size
changeScreenSize :: V2 Int -> ViewSettings -> ViewSettings
changeScreenSize s viewSet = viewSet { screenSize = s }

-- Update the given viewSettings by adding the
-- the first argument to the current zoom
changeZoom :: Double -> ViewSettings -> ViewSettings
changeZoom iz viewS =
  let z = viewZoom viewS in
  viewS { viewZoom = z + iz }

--
-- Render Model
--

inGameToScreenCoord :: V2 Double -> ViewSettings -> V2 Double
inGameToScreenCoord (V2 x y) viewS =
  let (V2 offsetx offsety) = viewOffset viewS in
  let (V2 screenx screeny) = screenSize viewS in
  let (sx, sy) = (fromIntegral screenx, fromIntegral screeny) in
  let zoom = viewZoom viewS in
  V2 (zoom * (x - offsetx) + (sx / 2)) (zoom * (y - offsety) + (sy / 2))

-- Render text under the given object's coordinates and size
-- Takes the text, the object and the view settings
label :: String -> V2 Double -> Double -> ViewSettings -> Form e
label t p a viewS = move (inGameToScreenCoord (p + V2 0 (20 + a)) viewS)
                    $ text
                    $ HT.color (rgb 1 1 1)
                    $ HT.toText t

renderBody :: ViewSettings -> Body -> Form e
renderBody viewS b = group
  [ label (bname b) (bpos b) (size b) viewS
  , move (inGameToScreenCoord (bpos b) viewS)
    $ filled (color b)
    $ circle (size b * viewZoom viewS)
  ]

renderBodies :: ViewSettings -> [Body] -> Form e
renderBodies viewS bodies = group
  [ group $ L.map (renderBody viewS) bodies
  , group $ L.map (renderBodies viewS . cbodies) bodies
  ]

renderFleet :: ViewSettings -> Fleet -> Form e
renderFleet viewS f = group
  [ label (fname f) (fpos f) 0 viewS
  , move (inGameToScreenCoord (fpos f) viewS)
    $ filled (rgb 1 0 0)
    $ square (5 * viewZoom viewS)
  ]

renderFleets :: ViewSettings -> Map.HashMap String Fleet -> Form e
renderFleets viewS fleets = group $ L.map (renderFleet viewS . snd) $ Map.toList fleets

-- Render the history above the prompt, takes the y size of the screen
-- and a list of lines to display
renderHistory :: Double -> [String] -> [Form e]
renderHistory y =
  L.zipWith
  (\n s ->
    move (V2 0 (y - 40 - n * 20))
    $ text
    $ HT.alignBottomLeft
    $ HT.color (rgb 1 1 1)
    $ HT.toText s
  ) [0..]

-- Render the shell, takes the size of the screen and the shell to display
renderShell :: V2 Int -> Shell -> Form e
renderShell (V2 osx osy) (Shell p h) =
  let (sx, sy) = (fromIntegral osx, fromIntegral osy) in
  case p of
    Nothing  -> group []
    Just cmd ->
      let len = fromIntegral $ L.length h + 1 in
      let backgr = move (V2 (sx / 2) (sy - len * 10))
                   $ filled (rgb 0.1 0.1 0.1)
                   $ rect (V2 sx $ len * 20)
      in
      let prompt = move (V2 0 (sy - 20))
                   $ text
                   $ HT.alignBottomLeft
                   $ HT.color (rgb 1 1 1)
                   $ HT.toText ("> " ++ cmd)
      in
      group $ backgr : prompt : renderHistory sy h

view :: Model -> Graphics SDLEngine
view model =
  let viewS = viewSet model in
  let dsi = dispSysId viewS in
  let ls = systems model in
  Graphics2D $ collage $
    if L.length ls - 1 < dsi
      then
        []
      else
        [ renderBodies viewS [sun $ ls L.!! dsi]
        , renderFleets viewS (Map.filter (\f -> fSysId f == dsi) (fleets model))
        , renderShell (screenSize viewS) (shell model)
        ]
