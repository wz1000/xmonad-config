import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS ( WSType(..), Direction1D(..), moveTo, toggleWS')
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D ( withNavigation2DConfig
                                   , centerNavigation
                                   , Navigation2DConfig(..)
                                   , windowGo
                                   , windowSwap
                                   , Direction2D(..)
                                   , switchLayer)
import XMonad.Actions.Volume (lowerVolume, raiseVolume, toggleMute)
import XMonad.Actions.SpawnOn
import XMonad.Layout.Fullscreen ( fullscreenEventHook )
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.SetWMName   ( setWMName )
import XMonad.Hooks.ManageDocks ( avoidStruts, docks, manageDocks )
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Layout.BinarySpacePartition ( emptyBSP )
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.BorderResize ( borderResize )
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle ( Toggle(..), mkToggle1 )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(NBFULL) )
import XMonad.Layout.NoBorders  ( smartBorders )
import XMonad.Prompt.Shell  ( shellPrompt )
import XMonad.Prompt.Window  ( windowPromptGoto )
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste ( pasteSelection )

import Spacing

import Control.Monad (void, liftM2)

startupApps = ["dunst"
              ,"pkill compton; compton"
              ,"feh --randomize --bg-fill ~/.wallpapers/*"
              ,"emacs --daemon"
              ,"pkill polybar; polybar example"
              ]

main :: IO ()
main = do
    xmonad
     $ withNavigation2DConfig ( def { defaultTiledNavigation = centerNavigation } )
     $ docks
     $ ewmh def
        { terminal        = "termite"
        , borderWidth     = 2
        , modMask         = mod4Mask
        , layoutHook      = myLayout
        , workspaces      = myWorkspaces
        , manageHook      = myManageHook
        , handleEventHook = fullscreenEventHook
        , startupHook     = setWMName "LG3D" >> mapM_ spawn startupApps
        } `additionalKeysP` myKeyBindings

myWorkspaces =
    zipWith (\i w -> show i ++ ". " ++ w)
            [1..]
            ["Main"
            ,"IRC"
            ,"Web"
            ,"Dev"
            ,"Conf"
            ,"Play"
            ,"Media"
            ]

myLayout = smartBorders
         $ gaps (zip [U,L,D,R] [0,0,0,0])
         $ borderResize
         $ mkToggle1 NBFULL
         $ avoidStruts layouts
          where
              layouts = spacing 15 emptyBSP

scratchpads = 
  [ NS "pavucontrol" "pavucontrol" (resource =? "pavucontrol") defaultFloating
  , NS "ncmpcpp" "termite --class=ncmpcpp -e ncmpcpp" (className =? "ncmpcpp") defaultFloating
  , NS "wicd" "wicd-client --no-tray" (resource =? "wicd-client.py") defaultFloating
  , NS "clerk" "termite --class=clerk -e clerk" (className =? "clerk") defaultFloating
  ]

manageApps = composeAll
    [ isFullscreen                     --> doFullFloat
    , resource =? "dmenu"              --> doFloat
    , resource =? "pavucontrol"        --> placeHook ( fixed (1,85/1080) ) <+> doFloat
    , resource =? "wicd-client.py"     --> placeHook ( fixed (1,85/1080) ) <+> doFloat
    , resource =? "gsimplecal"         --> placeHook ( fixed (1,35/1080) )
    , resource =? "htop"               --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "alsamixer"          --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "nethogs"            --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "ncmpcpp"            --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "progress"           --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "runner"             --> placeHook ( fixed (0,1) ) <+> doFloat
    , resource =? "feh"                --> doIgnore
    , resource =? "dzen2"              --> doIgnore
    , resource =? "polybar"            --> doIgnore
    , className =? "ncmpcpp"           --> doRectFloat (centerAligned 0.5 (30/1080) (2/3) 0.6)
    , className =? "clerk"             --> placeHook ( fixed (0.5,55/1080) ) <+> doFloat
    , manageDocks
    ]
    where
      {-
            x            x
            |            |
      y--+--+--+      y--+-----+
         |     |         |     |
         |     h   =>    |     h
         |     |         |     |
         +--w--+         +--w--+
      -}
      centerAligned x y w h =
        W.RationalRect (x-w/2) y w h

myManageHook = manageApps
          <+> placeHook (inBounds (underMouse (0.5, 0.5)))
          <+> manageHook def
          <+> scratchpadManageHookDefault
          <+> namedScratchpadManageHook scratchpads


restartXMonad = concat
  ["cd ~/.xmonad/; "
  ,"stack install; "
  ,"rm xmonad-x86_64-linux; "
  ,"cp ~/.local/bin/xmonad xmonad-x86_64-linux; "
  ,"cd ~; "
  ,"xmonad --restart; "
  ]

myKeyBindings = concat
    [ xmonadControlBindings
    , appLaunchBindings
    , windowMoveBindings
    , mediaBindings
    , bspControlBindings
    ]

appLaunchBindings =
    [("M-S-t", spawn "termite")
    ,("M-<Return>", spawn "termite")
    ,("M-S-b", spawn "firefox")
    ,("M-S-k", spawn "xkill")
    ,("M-S-e", spawn "emacsclient -c")
    ,("M-g", scratchpadSpawnActionCustom "termite --name scratchpad")
    ,("<Insert>", pasteSelection )
    ,("<F10>", namedScratchpadAction scratchpads "ncmpcpp")
    ,("M-<F11>", namedScratchpadAction scratchpads "pavucontrol")
    ,("M-<F12>", namedScratchpadAction scratchpads "wicd")
    ]

xmonadControlBindings =
    [("M-q", spawn restartXMonad)
    ,("M-r", shellPrompt def)
    ,("M-n", moveTo Next NonEmptyWS)
    ,("M-p", moveTo Prev NonEmptyWS)
    ,("M-c", moveTo Next EmptyWS)
    ,("M-`", toggleWS' ["NSP"])
    ,("M-S-x",kill)
    ,("M-C-d", sendMessage $ SPACING $ negate 5)
    ,("M-C-i", sendMessage $ SPACING 5)
    ,("M-;", gridselectWorkspace def W.view)
    ,("M-S-;", gridselectWorkspace def (W.shift))
    ,("M-S-C-;", gridselectWorkspace def (liftM2 (.) W.view W.shift))
    ,("M-f", sendMessage $ Toggle NBFULL)
    ,("M-/", windowPromptGoto def)
    ,("M-<Space>", switchLayer)
    ]

mediaBindings =
    [("<XF86AudioNext>", spawn "mpc next")
    ,("<XF86AudioPrev>", spawn "mpc prev")
    ,("<XF86AudioPlay>", spawn "mpc toggle")
    ,("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    ,("<XF86AudioLowerVolume>", spawn "~/scripts/dvol2 -d 2")
    ,("<XF86AudioRaiseVolume>", spawn "~/scripts/dvol2 -i 2")
    ,("<XF86MonBrightnessUp>", spawn "light -A 5")
    ,("M-<Right>", spawn "light -A 5")
    ,("<XF86MonBrightnessDown>", spawn "light -U 5")
    ,("M-<Left>", spawn "light -U 5")
    ,("M-m", spawn "clerk -t")
    ]

windowMoveBindings = do
    (key,dir) <- zip "hjkl" [L,D,U,R]
    id [("M-"  ++[key], windowGo   dir False)
       ,("M-S-"++[key], windowSwap dir False)
       ]

bspControlBindings = 
    [("M-b e", sendMessage BSP.Equalize)
    ,("M-b r", sendMessage BSP.Rotate)
    ,("M-b s", sendMessage BSP.Swap)
    ,("M-b p", sendMessage BSP.FocusParent)
    ,("M-M1-h", sendMessage $ BSP.ExpandTowards L)
    ,("M-M1-j", sendMessage $ BSP.ExpandTowards D)
    ,("M-M1-k", sendMessage $ BSP.ExpandTowards U)
    ,("M-M1-l", sendMessage $ BSP.ExpandTowards R)
    ,("M-C-h", sendMessage $ BSP.MoveSplit L)
    ,("M-C-j", sendMessage $ BSP.MoveSplit D)
    ,("M-C-k", sendMessage $ BSP.MoveSplit U)
    ,("M-C-l", sendMessage $ BSP.MoveSplit R)
    ]
