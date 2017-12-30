module View (view, changeOffset) where

import           Helm
import           Linear.V2        (V2(V2))
import           Helm.Color
import           Helm.Graphics2D
import           Helm.Engine.SDL  (SDLEngine)
import qualified Helm.Graphics2D.Text as HT
import qualified Data.List        as L

import           Types

changeOffset ix iy (V2 x y) = V2 (x + ix) (y + iy)

inGameToScreenCoord :: V2 Double -> V2 Double -> V2 Int -> Double -> V2 Double
inGameToScreenCoord (V2 x y) (V2 offsetx offsety) (V2 screenx screeny) zoom = V2 (zoom * (x - offsetx) + (fromIntegral screenx / 2)) (zoom * (y - offsety) + (fromIntegral screeny / 2))

label t p a offset ss zoom = [move (inGameToScreenCoord (p + (V2 0 (20 + a))) offset ss zoom) $ text $ HT.color (rgb 1 1 1) $ HT.toText $ t]

renderBody zoom offset ss b = toForm $ collage $ (label (bname b) (bpos b) (size b) offset ss zoom) ++ [move (inGameToScreenCoord (bpos b) offset ss zoom) $ filled (color b) $ circle ((size b) * zoom)]

renderBodies zoom offset ss b = toForm $ collage $ (L.map (renderBody zoom offset ss) b) ++ (L.map ((renderBodies zoom offset ss) . cbodies) b)

renderFleet zoom offset ss f = toForm $ collage $ (label (fname f) (fpos f) 0 offset ss zoom) ++ [(move (inGameToScreenCoord (fpos f) offset ss zoom) $ filled (rgb 1 0 0) $ square (5 * zoom))]

renderFleets zoom offset ss f = toForm $ collage $ L.map (renderFleet zoom offset ss) f

renderPrompt (V2 sx sy) p =
  case p of
    Nothing     -> []
    Just prompt -> [move (V2 (fromIntegral sx / 2) 0) $ filled (rgb 0.1 0.1 0.1) $ rect (V2 (fromIntegral sx) 40)] ++ [move (V2 0 0) $ text $ HT.alignBottomLeft $ HT.color (rgb 1 1 1) $ HT.toText prompt]

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
        [renderBodies zoom offset ss [sun $ ls L.!! dsi]] ++
        [renderFleets zoom offset ss $ (L.filter (\f -> fSysId f == dsi) $ fleets model)] ++
        (renderPrompt ss $ prompt model)
