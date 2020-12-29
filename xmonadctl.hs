import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Environment
import GHC.ResponseFile

main :: IO ()
main = sendCommand  "XMONAD_COMMAND" . escapeArgs =<< getArgs

sendCommand :: String -> String -> IO ()
sendCommand addr s = do
  d   <- openDisplay ""
  rw  <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d s False
  allocaXEvent $ \e -> do
                  setEventType e clientMessage
                  setClientMessageEvent e rw a 32 m currentTime
                  sendEvent d rw False structureNotifyMask e
                  sync d False
