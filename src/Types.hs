module Types
       (
         Action(..),
         Body(..),
         Ship(..),
         Fleet(..),
         SolarSystem(..),
         Model(..),
       ) where

import           Helm.Color
import qualified Helm.Keyboard as KB

import           Linear.V2     (V2(V2))

data Action = Tick Double | WindowResized (V2 Int) | KeyPressed KB.Key

data Body = Body
  {
    x       :: Double,
    y       :: Double,
    dfp     :: Double,
    curOr   :: Double,
    color   :: Color,
    size    :: Double,
    cbodies :: [Body]
  }

data Ship = Ship
  {
    hp              :: Int,
    blah            :: String
  }

data Fleet = Fleet
  {
    fx               :: Double,
    fy               :: Double,
    speed            :: Int,
    ships            :: [Ship],
    fdestx           :: Double,
    fdesty           :: Double
}

data SolarSystem = SolarSystem
  {
    sun             :: Body,
    fleets          :: [Fleet]
  }

data Model = Model
  {
    systems     :: [SolarSystem],
    dispSysId   :: Int,
    screenSize  :: V2 Int,
    viewOffset  :: V2 Double,
    viewZoom    :: Double
  }


