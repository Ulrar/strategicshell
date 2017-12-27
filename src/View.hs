module View (view) where

import           Helm
import           Linear.V2        (V2(V2))
import           Helm.Color
import           Helm.Graphics2D
import           Helm.Engine.SDL  (SDLEngine)
import qualified Data.List        as L

import           Types

inGameToScreenCoord :: Double -> Double -> V2 Double -> V2 Int -> Double -> V2 Double
inGameToScreenCoord x y (V2 offsetx offsety) (V2 screenx screeny) zoom = V2 (zoom * (x - offsetx) + (fromIntegral screenx / 2)) (zoom * (y - offsety) + (fromIntegral screeny / 2))

renderBody zoom offset ss b = move (inGameToScreenCoord (x b) (y b) offset ss zoom) $ filled (color b) $ circle ((size b) * zoom)

renderBodies zoom offset ss b = toForm $ collage $ (L.map (renderBody zoom offset ss) b) ++ (L.map ((renderBodies zoom offset ss) . cbodies) b)

view :: Model -> Graphics SDLEngine
view model =
  let zoom = viewZoom model in
  let offset = viewOffset model in
  let dsi = dispSysId model in
  let ss = screenSize model in
  let ls = systems model in
  Graphics2D $ collage $
    if (L.length ls) - 1 < dsi
      then
        []
      else
        [renderBodies zoom offset ss [sun $ ls L.!! dsi]]
