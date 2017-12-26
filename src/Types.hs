module Types
       (
         Action(..),
         Body(..),
         Ship(..),
         Fleet(..),
         SolarSystem(..),
         Model(..),
       ) where

import qualified Helm.Keyboard as KB

import           Linear.V2     (V2(V2))

data Action = Tick Double | WindowResized (V2 Int) | KeyPressed KB.Key

data Body = Body
  {
    x       :: Double,
    y       :: Double,
    dfp     :: Double,
    curOr   :: Double,
    radius  :: Double,
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
    ships            :: [Ship]
  }

data SolarSystem = SolarSystem
  {
    bodies          :: [Body],
    fleets          :: [Fleet]
  }

data Model = Model
  {
    systems     :: [SolarSystem],
    dispSysId   :: Int,
    size        :: V2 Int,
    offset      :: V2 Double,
    zoom        :: Double
  }


