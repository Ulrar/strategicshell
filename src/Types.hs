module           Types
                 ( Action(..)
                 , Body(..)
                 , Ship(..)
                 , Fleet(..)
                 , SolarSystem(..)
                 , ViewSettings(..)
                 , Shell(..)
                 , Model(..)
                 )
where

import           Helm.Color
import qualified Helm.Keyboard        as KB

import           Linear.V2            (V2(V2))
import           Data.HashMap.Strict

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

data Shell = Shell
  {
    prompt      :: Maybe String,
    history     :: [String]
  }

data Model = Model
  {
    systems     :: [SolarSystem],
    fleets      :: HashMap String Fleet,
    viewSet     :: ViewSettings,
    shell       :: Shell
  }


