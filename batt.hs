import System.IO
import Control.Concurrent (threadDelay)
import Control.DeepSeq (rnf)
import Data.Time.Clock
import Graphics.UI.Gtk
import Text.Printf
import Text.Regex.PCRE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

iconPrefix     = "/usr/share/icons/gnome/scalable/status/"
battFullip     = iconPrefix ++ "battery-full-charged-symbolic.svg"
battFullipc    = iconPrefix ++ "battery-full-charging-symbolic.svg"
battGoodip     = iconPrefix ++ "battery-good-symbolic.svg"
battGoodipc    = iconPrefix ++ "battery-good-charging-symbolic.svg"
battLowip      = iconPrefix ++ "battery-low-symbolic.svg"
battLowipc     = iconPrefix ++ "battery-low-charging-symbolic.svg"
battCautionip  = iconPrefix ++ "battery-caution-symbolic.svg"
battCautionipc = iconPrefix ++ "battery-caution-charging-symbolic.svg"
battEmptyip    = iconPrefix ++ "battery-empty-symbolic.svg"
battEmptyipc   = iconPrefix ++ "battery-empty-charging-symbolic.svg"

battDir           = "/sys/bus/acpi/drivers/battery/PNP0C0A:00/power_supply/BAT0/"
battCapFile       = battDir ++ "capacity"
battStatFile      = battDir ++ "status"
battEnergyNowFile = battDir ++ "energy_now"

-- This is how long we wait in seconds between battery energy level samples.
-- The longer we wait the more accurate but also the more out of date the rate
-- used when it is finally used. This should be tuned.
delay = 10

normalizeCapacity :: B.ByteString -> Int
normalizeCapacity rawCapacity = 
  case (BC.readInt rawCapacity) of
    Just (i,_) -> i
    _ -> 0

normalizeEnergy :: B.ByteString -> Int
normalizeEnergy = normalizeCapacity

normalizeStatus :: B.ByteString -> String
normalizeStatus rawStatus =
  let s = BC.unpack rawStatus in
  let fs = (s =~ "[^\n]*" :: String) in
  if (fs == "Unknown") then 
    "Charging"
  else
    fs
    
chooseIcon :: B.ByteString -> B.ByteString -> String
chooseIcon rawStatus rawCapacity =
  let status = normalizeStatus rawStatus in
  let capacity = normalizeCapacity rawCapacity in
  case (capacity,status) of
  (x,y) | x > 75 && y == "Full"        -> battFullipc
  (x,y) | x > 75 && y == "Charging"    -> battFullipc
  (x,y) | x > 75 && y == "Discharging" -> battFullip 
  (x,y) | x > 50 && y == "Charging"    -> battGoodipc
  (x,y) | x > 50 && y == "Discharging" -> battGoodip  
  (x,y) | x > 25 && y == "Charging"    -> battLowipc 
  (x,y) | x > 25 && y == "Discharging" -> battLowip 
  (x,y) | x > 10 && y == "Charging"    -> battCautionipc 
  (x,y) | x > 10 && y == "Discharging" -> battCautionip 
  (x,y) | x < 10 && y == "Charging"    -> battEmptyipc 
  (x,y) | x < 10 && y == "Discharging" -> battEmptyip
  (_,_)                                -> battEmptyip -- Should probably have a question-mark icon for this unknown case

getTooltipText :: Float -> B.ByteString -> B.ByteString -> B.ByteString -> String
getTooltipText drainRate rawEnergyNow stat cap =
  let nstat = normalizeStatus stat in
  let ncap = normalizeCapacity cap in
  let energyNow = normalizeEnergy rawEnergyNow in
  let ts = ((3600 * (fromIntegral energyNow)) / drainRate) in
  let h = floor (ts / 3600) :: Int in
  let m = floor ((ts - (fromIntegral h*3600))/60) :: Int in
  let s = floor ((ts - (fromIntegral h*3600) - (fromIntegral m*60))/60) :: Int in
  case nstat of
    "Discharging" -> printf "Discharging:%d%%, %02d:%02d:%02d remaining" ncap h m s
    x -> printf "%s:%d%%" x ncap

iconUpdater :: StatusIcon -> MVar Float -> IO Bool
iconUpdater si mv = do
  rawCapacity   <- B.readFile battCapFile 
  rawStatus     <- B.readFile battStatFile 
  rawEnergyNow  <- B.readFile battEnergyNowFile
  drainRate     <- readMVar mv 
  statusIconSetFromFile si (chooseIcon rawStatus rawCapacity)
  statusIconSetTooltip si (getTooltipText drainRate rawEnergyNow rawStatus rawCapacity) 
  return True 

battDrainRate :: Int -> B.ByteString -> B.ByteString -> Float 
battDrainRate d en1 en2 =
  case ((BC.readInt en1),(BC.readInt en2)) of
    ((Just (i1,_)),(Just (i2,_))) -> (fromIntegral (i1 - i2)*((60/fromIntegral d)*60)) 
    (_,_) -> 0

tooltipUpdater :: MVar Float -> IO () 
tooltipUpdater mv = do
  rawEnergyNow1 <- B.readFile battEnergyNowFile
  threadDelay (1000000 * delay) 
  rawEnergyNow2 <- B.readFile battEnergyNowFile
  modifyMVar_ mv (\_ -> (return (battDrainRate delay rawEnergyNow1 rawEnergyNow2)))

main :: IO ()
main = do
  drainRateMV <- newMVar 0.0 
  initGUI
  si <- statusIconNewFromStock stockQuit
  iconUpdater si drainRateMV 
  statusIconSetVisible si True
  ti0 <- timeoutAdd (yield >> return True) 50   -- Gives tooltipUpdater thread a chance to run 
  ti2 <- timeoutAdd (iconUpdater si drainRateMV) 5000
  forkIO (forever $ tooltipUpdater drainRateMV) 
  menu <- mkmenu si
  on si statusIconPopupMenu $ \b a -> do
         widgetShowAll menu
         print (b,a)
         menuPopup menu $ maybe Nothing (\b' -> Just (b',a)) b
  mainGUI

mkmenu s = do
  m <- menuNew
  mapM_ (mkitem m) [("Quit",mainQuit)]
  return m
    where
        mkitem menu (label,act) =
            do i <- menuItemNewWithLabel label
               menuShellAppend menu i
               on i menuItemActivated act
