module View
       ( view
       , changeOffset
       , changeScreenSize
       , changeZoom
       ) where

import           Helm
import           Linear.V2        (V2(V2))
import           Helm.Color
import           Helm.Graphics2D
import           Helm.Engine.SDL  (SDLEngine)
import qualified Helm.Graphics2D.Text as HT
import qualified Data.List        as L

import           Types

--
-- Update ViewSettings
--
changeOffset ix iy viewS =
  let (V2 x y) = viewOffset viewS in
  viewS { viewOffset = V2 (x + ix) (y + iy) }

changeScreenSize s viewSet = viewSet { screenSize = s }

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
  let zoom = viewZoom viewS in
  V2 (zoom * (x - offsetx) + (fromIntegral screenx / 2)) (zoom * (y - offsety) + (fromIntegral screeny / 2))

label t p a viewS = move (inGameToScreenCoord (p + V2 0 (20 + a)) viewS)
                    $ text
                    $ HT.color (rgb 1 1 1)
                    $ HT.toText t

renderBody viewS b = group
  [ label (bname b) (bpos b) (size b) viewS
  , move (inGameToScreenCoord (bpos b) viewS)
    $ filled (color b)
    $ circle (size b * viewZoom viewS)
  ]

renderBodies viewS b = group
  [ group $ L.map (renderBody viewS) b
  , group $ L.map (renderBodies viewS . cbodies) b
  ]

renderFleet viewS f = group
  [ label (fname f) (fpos f) 0 viewS
  , move (inGameToScreenCoord (fpos f) viewS)
    $ filled (rgb 1 0 0)
    $ square (5 * viewZoom viewS)
  ]

renderFleets viewS f = group $ L.map (renderFleet viewS) f

-- Render the history on top of the prompt, takes the y size of the screen
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
renderShell (V2 osx osy) (Shell p h) =
  let (sx, sy) = (fromIntegral osx, fromIntegral osy) in
  case p of
    Nothing     -> group []
    Just prompt -> group $
      [ move (V2 (sx / 2) (sy - 10 - fromIntegral (L.length h * 10))) --
        $ filled (rgb 0.1 0.1 0.1)                                    -- Background
        $ rect (V2 sx $ fromIntegral (L.length h * 20) + 20)          --
      , move (V2 0 (sy - 20))
        $ text                                                        --
        $ HT.alignBottomLeft                                          -- Current prompt
        $ HT.color (rgb 1 1 1)                                        -- with > in front
        $ HT.toText ("> " ++ prompt)                                  --
      ] ++
      renderHistory sy h

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
        , renderFleets viewS (L.filter (\f -> fSysId f == dsi) (fleets model))
        , renderShell (screenSize viewS) (shell model)
        ]
