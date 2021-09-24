{-# LANGUAGE ViewPatterns #-}
module Main where
  
import Xmobar
import System.Environment

barConfig :: Int -> Config
barConfig n = defaultConfig {
  font = "xft:mononoki Nerd Font Mono:weight=bold:antialias=false:size=10"
  , additionalFonts = ["xft:Siji:size=12", "xft:mononoki Nerd Font Mono:weight=bold:antialias=false:size=14", "xft:mononoki Nerd Font Mono:weight=bold:antialias=false:size=12"]
  , borderColor = "black"
  , border = NoBorder
  , borderWidth = 0
  , bgColor = "#2b2b2b"
  , fgColor = "grey"
  , alpha = 255
  , position = OnScreen n $ Bottom
  , textOffset = 12
  , textOffsets = [12, 13,14]
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , commands = [ Run $ Cpu ["-L","3","-H","50",
                             "--normal","green","--high","red"] 10
               , Run $ Memory ["-t","Mem: <usedratio>%"] 10
               , Run $ Swap [] 10
               , Run $ Date "%a %b %_d %H:%M" "date" 50
               , Run UnsafeStdinReader
               , Run $ Mail [("gmail","~/Mail/gmail/INBOX/")
                            ,("wt", "~/Mail/well-typed/INBOX/")
                            ]
                            "mail"
               -- , Run $ QueueReader q id "UnsafeStdinReader"
               , Run $ DiskIO [("nvme0n1p2","nvme <read> <write>"),("sda1","sata <read> <write>")] [] 10
              ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%UnsafeStdinReader% } { %diskio% | %mail% | %date%"
}

main = do
  [read -> scrn] <- getArgs
  xmobar (barConfig scrn)

