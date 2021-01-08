module Main where

import Control.Concurrent (threadDelay)
import System.FilePath.Posix
import System.Directory
import qualified Data.Map as Map
import Data.Char
import Data.List
import Text.Printf

-- the type of a thermal zone record

data Zone = Zone
  { zoneType :: String
  , zonePath :: FilePath
  , zoneTarget :: Int
  , lastTemp :: Int
  } deriving (Show)

-- typeclass for formatted output a la Show
class Display a where
  format :: a -> String

instance Display Zone where
  format z = printf "%15s...%4.1f/%4.1f°C" (zoneType z) t zt where
    t, zt :: Float
    t = fromIntegral (lastTemp z) / 1000.0
    zt = fromIntegral (zoneTarget z) / 1000.0                                               

--type ZoneMap = Map.Map String Zone 

thermalClass :: FilePath
thermalClass = "/sys/devices/virtual/thermal"

thermalPrefix :: String
thermalPrefix = "thermal_"

zoneTempPath :: FilePath -> FilePath
zoneTempPath zone = thermalClass </> zone </> "temp"

zoneTypePath :: FilePath -> FilePath
zoneTypePath zone = thermalClass </> zone </> "type"

-- entry point
-- TODO config and command line opts
main :: IO ()
main = do
  putStrLn "Fan control v0.1-alpha - (c) Simon Beaumont 2019"
  
  zoneList <- getThermalZones
  let zoneTempPaths = map zoneTempPath zoneList
  
  zoneTypes <- sequence $ map (getZoneType . zoneTypePath) zoneList
  let zones = map makeZone (zip zoneTypes zoneList)

  -- just one shared fan for now but zone could have its own fan
  fan <- driveFanLevel (Fan fanControl 0) 1

  putStrLn "monitoring temperature zones and driving fans..."
  -- go to it
  monitorZones zones fan


-- monitor zone temps and adjust fan
monitorZones :: [Zone] -> Fan -> IO ()
monitorZones zs f = do
  zones <- sequence $ map updateZone zs
  fan <- if any isHot zones
    then driveFanLevel f 1 
    else driveFanLevel f (-1)
  mapM_ (putStrLn . format) zones
  putStrLn $ format fan
  -- wait
  threadDelay (10*1000000)
  monitorZones zones fan


-- get the thermal zone files from the system
getThermalZones :: IO [FilePath]
getThermalZones = filter (isPrefixOf thermalPrefix) <$> listDirectory thermalClass

-- get the zone type
getZoneType :: FilePath -> IO String
getZoneType p = readFile p >>= pure . dropWhileEnd isSpace


-- make zone record from path
makeZone :: (String, FilePath) -> Zone
makeZone (s,p) = Zone
  { zoneType = s 
  , zonePath = zoneTempPath p
  , zoneTarget = 36000
  , lastTemp = 0
  }

-- update zone temperature record
-- N.B. there is an artifact where some zone temp is clamped at 100000 or 100 degrees!
updateZoneTemp :: Zone -> Int -> Zone
updateZoneTemp z n = z {lastTemp = if n < 100000 then n else 0}

-- zone is hot
isHot :: Zone -> Bool
isHot z = lastTemp z > zoneTarget z
  
-- read current zone temperature from sys
getZoneTemp :: Zone -> IO Int
getZoneTemp z = do
  zt <- readFile $ zonePath z
  return $ read zt

-- update zone with current temp
updateZone :: Zone -> IO Zone
updateZone z = getZoneTemp z >>= (return . updateZoneTemp z)
               

-- fan control port YMMV -- probably needs root access for writing
fanControl :: FilePath
fanControl = "/sys/devices/pwm-fan/target_pwm"

data Fan = Fan
  { fanPort :: FilePath
  , fanLevel :: Int
  } deriving (Show)

instance Display Fan where
  format f = printf "%15s...%2d/%2d" "fan level" (fanLevel f) (length fanSpeeds - 1) 
  
-- fan speeds in sensible increments
fanSpeeds :: [Int]
fanSpeeds = nub $ [0,32..255] ++ [255]

-- set fan speed to an speed
writeFanPort :: FilePath -> Int -> IO ()
writeFanPort p n = writeFile p (show n) 

-- update fan level by a signed offset
updateFanLevel :: Fan -> Int -> Fan
updateFanLevel f i = f { fanLevel = clampLevel i } where
  clampLevel :: Int -> Int
  clampLevel i' =
    let tl = fanLevel f + i' -- target level
        ub = min (length fanSpeeds - 1)
        lb = max 0
    in (ub . lb) tl

-- drive fan up or down
driveFanLevel :: Fan -> Int -> IO Fan
driveFanLevel f i =
  let f' = updateFanLevel f i in
    writeFanPort (fanPort f') (fanSpeeds !! fanLevel f') >> return f'

  
-- set fan and wait s seconds
setFanAndWait :: Int -> Int -> IO ()
setFanAndWait s n = writeFanPort fanControl n >> threadDelay (s * 1000000)

-- fun start up/test function
playFanny :: IO ()
playFanny = mapM_ (setFanAndWait 10) fanSpeeds  



