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

renderBody zoom offset ss (Body x y _ _ r b) = move (inGameToScreenCoord x y offset ss zoom) $ filled (rgb 1 0 0) $ circle (r * zoom)

renderBodies zoom offset ss b = toForm $ collage $ (L.map (renderBody zoom offset ss) b) ++ (L.map ((renderBodies zoom offset ss) . cbodies) b)

view :: Model -> Graphics SDLEngine
view (Model lsolsys dsi ss offset zoom) = Graphics2D $ collage $ [move (inGameToScreenCoord 0 0 offset ss zoom) $ filled (rgb 1 1 1) $ circle (20 * zoom)] ++
  (if (L.length lsolsys) - 1 < dsi then [] else [renderBodies zoom offset ss (bodies $ lsolsys L.!! dsi)])


