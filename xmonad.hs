import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS ( WSType(..), Direction1D(..), moveTo, toggleWS')
import XMonad.Actions.CopyWindow
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
import XMonad.Actions.Submap
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
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
import XMonad.Prompt.Window
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.ExtensibleState as ES
import XMonad.Util.Paste

import Spacing

import Control.Monad (void, liftM2)
import qualified Data.Map as M

startupApps = ["dunst"
              ,"pkill compton; compton"
              ,"feh --randomize --bg-fill ~/.wallpapers/*"
              ,"emacs --daemon"
              ,"pkill polybar; polybar example"
              ]

main :: IO ()
main = do
    xmonad
     $ dynamicProjects []
     $ withNavigation2DConfig ( def { defaultTiledNavigation = centerNavigation } )
     $ docks
--     $ modal regularMode
     $ addModal
     $ ewmh def
        { terminal        = "kitty"
        , borderWidth     = 2
        , focusedBorderColor = "#cccccc"
        , normalBorderColor = "#2b2b2b"
        , modMask         = mod4Mask
        , layoutHook      = myLayout
        , workspaces      = myWorkspaces
        , manageHook      = myManageHook
        , handleEventHook = fullscreenEventHook
        , startupHook     = setWMName "LG3D" >> mapM_ spawn startupApps
        } `additionalKeysP` myKeyBindings
          `removeKeysP` [pref ++ [n] | pref <- ["M-S-","M-"], n <- ['1'..'9']]

addModal c = additionalKeysP c [("M-.", enterNavMode c)]

myWorkspaces =
--    zipWith (\i w -> show i ++ ". " ++ w)
--            [1..]
            ["main"
            ,"irc"
            ,"web"
            ,"dev"
            ,"conf"
            ,"play"
            ,"media"
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
  , NS "ncmpcpp" "kitty --class=ncmpcpp -e ncmpcpp" (className =? "ncmpcpp") defaultFloating
  , NS "wicd" "wicd-client --no-tray" (resource =? "wicd-client.py") defaultFloating
  , NS "clerk" "kitty --class=clerk -e clerk" (className =? "clerk") defaultFloating
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
    -- , resource =? "ncmpcpp"            --> placeHook ( fixed (1,35/1080) ) <+> doFloat
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
    , windowBindings
    , mediaBindings
    ]

{-
regularMode c = Mode "regular" GrabBound $ mkKeymap c $
  [("M-.", setTo (navMode c))
  ]

navMode c = Mode "nav" GrabAll $ mkKeymap c $ concat
  [ windowKeys 
  , bspKeys
  , xmonadControlKeys
  , [("<Esc>", setTo (regularMode c))]
  ]
-}

enterNavMode c = mkModalBindings c "<Esc>" $ concat
  [ windowKeys 
  , bspKeys
  , xmonadControlKeys
  ]

appLaunchBindings =
    [("M-S-t", spawn "kitty")
    ,("M-<Return>", spawn "kitty")
    ,("M-S-b", spawn "firefox")
    ,("M-S-k", spawn "xkill")
    ,("M-S-e", spawn "emacsclient -c")
    ,("M-g", scratchpadSpawnActionCustom "kitty --name scratchpad")
    ,("<Insert>", pasteSelection )
    ,("<F10>", namedScratchpadAction scratchpads "ncmpcpp")
    ,("M-<F11>", namedScratchpadAction scratchpads "pavucontrol")
    ,("M-<F12>", namedScratchpadAction scratchpads "wicd")
    ,("M-q", spawn restartXMonad)
    ,("M-r", shellPrompt myXPConfig)
    ]

xmonadControlBindings = addSuperPrefix xmonadControlKeys

xmonadControlKeys =
    [("n", moveTo Next NonEmptyWS)
    ,("p", moveTo Prev NonEmptyWS)
    ,("`", toggleWS' ["NSP"])
    ,("S-x", kill1)
    ,("x", killCopy)
    ,("C-d", sendMessage $ SPACING $ negate 5)
    ,("C-i", sendMessage $ SPACING 5)
    ,("S-d", removeWorkspace)
    ,(";", switchProjectPrompt myXPConfig)
    ,("d", changeProjectDirPrompt myXPConfig)
    ,("w", shiftToProjectPrompt myXPConfig)
    ,("e", gridselectWorkspace def W.view)
    ,("f", sendMessage $ Toggle NBFULL)
    ,("/", windowPrompt highlightConfig Goto allWindows)
    ,("S-/", windowPrompt highlightConfig Bring allWindows)
    ,("\\", windowMultiPrompt highlightConfig [(BringCopy,allWindows),(Bring,allWindows)])
    ,("<Space>", switchLayer)
    ]

bspKeys =
  [("e", sendMessage BSP.Equalize)
  ,("r", sendMessage BSP.Rotate)
  ,("s", sendMessage BSP.Swap)
  ,("u", sendMessage BSP.FocusParent)
  ,("v", sendMessage BSP.MoveNode)
  ,("c", sendMessage BSP.SelectNode)
  ]

windowBindings = addSuperPrefix windowKeys

windowKeys = do
  (key,dir) <- zip "hjkl" [L,D,U,R]
  id [([key], windowGo   dir False)
     ,("S-"++[key], windowSwap dir False)
     ,("C-"++[key], sendMessage $ BSP.MoveSplit dir)
     ,("M1-"++[key], sendMessage $ BSP.ExpandTowards dir)
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

mkModalBindings :: XConfig l -> String -> [(String, X ())] -> X ()
mkModalBindings conf exitBind xs = enterMode
  where
    xs' = map (\(bind,action) -> (bind,action >> enterMode)) xs
    keymap = mkKeymap conf ((exitBind,return ()) : xs')
    enterMode = submapDefault enterMode keymap

myXPConfig = def { position = CenteredAt 0.5 0.5
                 , bgColor = "#333333"
                 , borderColor = "#303030"
                 , promptBorderWidth = 2
                 , font = "xft:Source Code Pro-10"
                 , height = 22
                 , searchPredicate = fuzzyMatch }
highlightConfig = myXPConfig{alwaysHighlight = True}

addSuperPrefix = map (\(b,a) -> ("M-"++b,a)) 

killCopy :: X ()
killCopy = do
    ss <- gets windowset 
    whenJust (W.peek ss) $ 
      \w -> if W.member w $ delete'' w ss
            then windows $ delete'' w
            else return ()
  where delete'' w = W.modify Nothing (W.filter (/= w))
