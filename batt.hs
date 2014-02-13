import System.IO
import Text.Regex.PCRE
import Control.DeepSeq (rnf)
import Graphics.UI.Gtk

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

getRawBattInfo :: IO (String, String)
getRawBattInfo = do
  ch <- openFile "/sys/bus/acpi/drivers/battery/PNP0C0A:00/power_supply/BAT0/capacity" ReadMode
  rawCapacity <- hGetContents ch
  sh <- openFile "/sys/bus/acpi/drivers/battery/PNP0C0A:00/power_supply/BAT0/status" ReadMode
  rawStatus <- hGetContents sh
  -- Need to force some strictness here so we can close the file handles.
  -- Another option is using strict IO provided by Bytestring, but that seems
  -- like overkill for only reading a few characters
  rnf rawCapacity `seq` hClose ch
  rnf rawStatus `seq` hClose sh
  return (rawStatus, rawCapacity)

normalizeCapacity :: String -> Int
normalizeCapacity rawCapacity = read (rawCapacity =~ "[^\n]*" :: String) :: Int

normalizeStatus :: String -> String
normalizeStatus rawStatus =
  let fs = (rawStatus =~ "[^\n]*" :: String) in
  if (fs == "Unknown") then 
    "Charging"
  else
    fs

tooltip :: String -> String -> String
tooltip status capacity = (normalizeStatus status) ++ ":" ++ show (normalizeCapacity capacity) ++ "%"

chooseIcon :: String -> String -> String
chooseIcon rawStatus rawCapacity =
  let status = normalizeStatus rawStatus in
  let capacity = normalizeCapacity rawCapacity in
  case (capacity,status) of
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

iconUpdater :: StatusIcon -> IO Bool
iconUpdater si = do
  (rawStatus,rawCapacity) <- getRawBattInfo 
  statusIconSetTooltip si (tooltip rawStatus rawCapacity)
  statusIconSetFromFile si (chooseIcon rawStatus rawCapacity) 
  return True 

main :: IO ()
main = do
  initGUI
  si <- statusIconNewFromStock stockQuit
  iconUpdater si
  statusIconSetVisible si True
  ti <- timeoutAdd (iconUpdater si) 5000
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
