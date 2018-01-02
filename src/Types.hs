module Types
       ( Action(..)
       , Body(..)
       , Ship(..)
       , Fleet(..)
       , SolarSystem(..)
       , ViewSettings(..)
       , Model(..)
       ) where

import           Helm.Color
import qualified Helm.Keyboard as KB

import           Linear.V2     (V2(V2))

data Action = Tick Double | WindowResized (V2 Int) | KeyPressed KB.Key

data Body = Body
  {
    bpos    :: V2 Double,
    bname   :: String,
    dfp     :: Double,
    curOr   :: Double,
    color   :: Color,
    size    :: Double,
    cbodies :: [Body]
  }

data Ship = Ship
  {
    hp              :: Int
  }

data Fleet = Fleet
  {
    fpos             :: V2 Double,
    fSysId           :: Int,
    speed            :: Int,
    ships            :: [Ship],
    fname            :: String,
    fdest            :: Maybe (V2 Double)
}

data SolarSystem = SolarSystem
  {
    sun             :: Body
  }

data ViewSettings = ViewSettings
  {
    dispSysId   :: Int,
    screenSize  :: V2 Int,
    viewOffset  :: V2 Double,
    viewZoom    :: Double
  }

data Model = Model
  {
    systems     :: [SolarSystem],
    fleets      :: [Fleet],
    viewSet     :: ViewSettings,
    cmdOutput   :: [String],
    prompt      :: Maybe String
  }


