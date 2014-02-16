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

delay = 10

normalizeCapacity :: B.ByteString -> Int
normalizeCapacity rawCapacity = 
  case (BC.readInt rawCapacity) of
    Just (i,_) -> i
    _ -> 0

normalizeStatus :: B.ByteString -> String
normalizeStatus rawStatus =
  let s = BC.unpack rawStatus in
  let fs = (s =~ "[^\n]*" :: String) in
  if (fs == "Unknown") then 
    "Charging"
  else
    fs
    
battTimeRemaining :: Int -> B.ByteString -> B.ByteString -> String
battTimeRemaining d en1 en2 =
  case ((BC.readInt en1),(BC.readInt en2)) of
    ((Just (i1,_)),(Just (i2,_))) ->
      let ts = ((3600 * fromIntegral i2) / (fromIntegral (i1 - i2)*((60/fromIntegral d)*60))) in
      let h = floor (ts / 3600) :: Int in
      let m = floor ((ts - (fromIntegral h*3600))/60) :: Int in
      let s = floor ((ts - (fromIntegral h*3600) - (fromIntegral m*60))/60) :: Int in
      printf ", %02d:%02d:%02d remaining" h m s
    (_,_) -> "" 

tooltip :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Int -> String
tooltip status capacity e1 e2 d = 
  printf "%s:%d%%%s" (normalizeStatus status) (normalizeCapacity capacity) (battTimeRemaining d e1 e2) 

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

--tooltipUpdater :: MVar String -> IO Bool 
--tooltipUpdater mv = do
--  rawCapacity   <- B.readFile battCapFile 
--  rawStatus     <- B.readFile battStatFile 
--  rawEnergynow1 <- B.readFile battEnergyNowFile
--  threadDelay (1000000 * delay) 
--  rawEnergynow2 <- B.readFile battEnergyNowFile
--  modifyMVar_ mv (\_ -> return (tooltip rawStatus rawCapacity rawEnergynow1 rawEnergynow2 delay))
--  return True

tooltipUpdater2 :: MVar String -> IO () 
tooltipUpdater2 mv = do
  rawCapacity   <- B.readFile battCapFile 
  rawStatus     <- B.readFile battStatFile 
  rawEnergynow1 <- B.readFile battEnergyNowFile
  threadDelay (1000000 * delay) 
  rawEnergynow2 <- B.readFile battEnergyNowFile
  modifyMVar_ mv (\_ -> return (tooltip rawStatus rawCapacity rawEnergynow1 rawEnergynow2 delay))
  tooltipUpdater2 mv

getIconPath :: IO String
getIconPath = do
  rawCapacity   <- B.readFile battCapFile 
  rawStatus     <- B.readFile battStatFile 
  return (chooseIcon rawStatus rawCapacity) 

iconUpdater :: StatusIcon -> MVar String -> IO Bool
iconUpdater si mv = do
  toolTip <- readMVar mv
  statusIconSetTooltip si toolTip
  iconPath <- getIconPath
  statusIconSetFromFile si iconPath
  return True 

main :: IO ()
main = do
  mv <- newMVar "test"
  initGUI
  si <- statusIconNewFromStock stockQuit
  iconUpdater si mv
  statusIconSetVisible si True
  ti0 <- timeoutAdd (yield >> return True) 50 
  --ti1 <- timeoutAdd (tooltipUpdater mv) 15000
  ti2 <- timeoutAdd (iconUpdater si mv) 5000
  forkIO (tooltipUpdater2 mv) 
  menu <- mkmenu si
  on si statusIconPopupMenu $ \b a -> do
         widgetShowAll menu
         print (b,a)
         menuPopup menu $ maybe Nothing (\b' -> Just (b',a)) b
  --on si statusIconActivate $ do
  --       putStrLn "'activate' signal triggered"
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
