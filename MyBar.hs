{-# LANGUAGE ViewPatterns #-}
module Main where
  
import Xmobar
import System.Environment

barConfig :: Int -> Config
barConfig n = defaultConfig {
  font = "mononoki Nerd Font Mono Bold 10"
  , additionalFonts = ["Siji 10.5", "mononoki Nerd Font Mono Bold 14", "mononoki Nerd Font Mono Bold 12"]
  , borderColor = "black"
  , border = NoBorder
  , borderWidth = 0
  , bgColor = "#2b2b2b"
  , fgColor = "#dfdfdf"
  , alpha = 255
  , position = OnScreen n $ Bottom
  , textOffset = 0
  , textOffsets = [0,0,0]
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , commands = [ Run $ Date "%a %b %_d %H:%M" "date" 50
               , Run UnsafeStdinReader
               , Run $ Mail [("gmail","~/Mail/gmail/Inbox/")
                            ,("wt", "~/Mail/well-typed/Inbox/")
                            ]
                            "mail"
               -- , Run $ QueueReader q id "UnsafeStdinReader"
               -- , Run $ DiskIO [("nvme0n1p2","root <read> <write>"),("nvme1n1p1","data <read> <write>")] [] 10
              ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%UnsafeStdinReader% }{  %mail% | %date%"
}

main = do
  [read -> scrn] <- getArgs
  xmobar (barConfig scrn)

