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
import XMonad.Layout.Simplest
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.SetWMName   ( setWMName )
import XMonad.Hooks.ManageDocks ( avoidStruts, docks, manageDocks )
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
import XMonad.Layout.BorderResize ( borderResize )
import XMonad.Layout.MultiToggle ( Toggle(..), mkToggle1 )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(NBFULL) )
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders  ( smartBorders )
import XMonad.Prompt.Shell  ( shellPrompt )
import XMonad.Prompt.Window
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Run ( safeSpawn )
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.ExtensibleState as ES
import XMonad.Util.Paste

import Spacing
import Modal hiding (defaultMode, normalMode, insertMode)
import qualified Modal as M

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.Monoid
import System.IO

startupApps = ["dunst"
              ,"pkill compton; compton"
              ,"pulseaudio --start"
              ,"feh --randomize --bg-fill ~/.wallpapers/*"
              ,"pkill polybar; polybar example"
              ]

runInTerm x = "kitty -e sh -c '" ++ x ++ "'"
runInTerm' x = "kitty --class mpvslave -e sh -c '" ++ x ++ "'"

sshCommand = runInTerm
  "TERM=xterm ssh -i ~/.ssh/google_compute_engine $(cat ~/.ssh/gcehost) -t tmux attach"

staticProjects :: [Project]
staticProjects =
  [ Project "web" "~" (Just $ spawnOn "web" "firefox")
  , Project "irc" "~" (Just $ spawnOn "irc" sshCommand)
  ]

main :: IO ()
main = do
    ps <- readDynamicProjects staticProjects
    forM_ [".xmonad-mode-log"] $ \file -> do
      safeSpawn "mkfifo" ["/tmp/" ++ file]

    xmonad
     $ dynamicProjects ps
     $ withNavigation2DConfig ( def { defaultTiledNavigation = centerNavigation } )
     $ docks
     $ modal regularMode
     $ (ewmh def
        { terminal        = "kitty"
        , borderWidth     = 2
        , focusedBorderColor = "#cccccc"
        , normalBorderColor = "#2b2b2b"
        , modMask         = mod4Mask
        , layoutHook      = myLayout
        , workspaces      = myWorkspaces
        , manageHook      = myManageHook
        , handleEventHook = fullscreenEventHook
        , logHook         = updateMode <> runAllPending
        , startupHook     = do
            setWMName "LG3D"
            mapM_ spawn startupApps
        } `additionalKeysP` myKeyBindings
          `additionalMouseBindings` myMouseBindings
          `removeKeysP` [pref ++ [n] | pref <- ["M-S-","M-"], n <- ['1'..'9']])

myMouseBindings = []

raiseFloating w = do
  isFloat <- gets $ M.member w . W.floating . windowset
  if isFloat
  then windows $ (W.shiftMaster .) $ W.focusWindow w
  else return ()

updateMode = do
  mode <- ES.gets M.label
  io $ appendFile "/tmp/.xmonad-mode-log" (mode ++ "\n")

myWorkspaces =
            ["main"
            ,"irc"
            ,"web"
            ,"dev"
            ,"conf"
            ,"play"
            ,"media"
            ]

myLayout = trackFloating $ smartBorders
         $ borderResize
         $ mkToggle1 NBFULL
         $ avoidStruts
         $ layouts
          where
              layouts = addTabs shrinkText myTabTheme
                $ subLayout [] Simplest myTall
              myTall = spacing 15 $ resizable 40 4
              resizable step n = ResizableTall 1 (1/step) ((1/2)+n/step) []
              -- ^ n is no of step-lengths right of center, which is used as the
              -- default split ratio
scratchpads =
  [ NS "pavucontrol" "pavucontrol" (resource =? "pavucontrol") defaultFloating
  , NS "ncmpcpp" "kitty --class=ncmpcpp -e ncmpcpp" (className =? "ncmpcpp") defaultFloating
  , NS "wicd" "wicd-client --no-tray" (resource =? "wicd-client.py") defaultFloating
  , NS "clerk" "kitty --class=clerk -e clerk" (className =? "clerk") defaultFloating
  ]

combineMPV :: X ()
combineMPV = withWindowSet $ \s -> do
  let winList = maybe [] W.integrate . W.stack . W.workspace . W.current $ s
  slaves <- filterM (runQuery $ className =? "mpvslave" ) winList
  masters <- filterM (runQuery $ resource =? "mpvytdl" ) winList
  case (slaves,masters) of
    ((slave:_),(master:_)) -> do
      sendMessage $ Merge master slave
    _ -> return ()

markMerge :: Monoid m => Query m
markMerge = do
  liftX $ addAction combineMPV
  return mempty

newtype PendingActions = PendingActions { getPending :: [X()] }
instance ExtensionClass PendingActions where
  initialValue = PendingActions []

addAction :: X () -> X ()
addAction x = ES.modify (\(PendingActions xs) -> PendingActions (x:xs))

runAllPending :: X ()
runAllPending = do
  PendingActions xs <- ES.get
  ES.put (PendingActions [])
  sequence_ xs

manageApps = composeAll
    [ isFullscreen                     --> doFullFloat
    , resource =? "dmenu"              --> doFloat
    , resource =? "mpvytdl"            --> markMerge
    , resource =? "pavucontrol"        --> placeHook ( fixed (1,85/1080) ) <+> doFloat
    , resource =? "wicd-client.py"     --> placeHook ( fixed (1,85/1080) ) <+> doFloat
    , resource =? "gsimplecal"         --> placeHook ( fixed (1,35/1080) )
    , resource =? "htop"               --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "alsamixer"          --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "nethogs"            --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "progress"           --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "runner"             --> placeHook ( fixed (0,1) ) <+> doFloat
    , resource =? "feh"                --> doIgnore
    , resource =? "dzen2"              --> doIgnore
    , resource =? "polybar"            --> doIgnore
    , resource =? "scratchpad"         --> doRectFloat (centerAligned 0.5 0.3 0.45 0.45)
    , resource =? "xmonadrestart"      --> doRectFloat (centerAligned 0.5 0.3 0.35 0.35)
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

myManageHook = composeAll
             [ raiseNew
             , insertPosition Below Newer
             , manageHook def
             , manageSpawn
             , manageApps
             , scratchpadManageHookDefault
             , namedScratchpadManageHook scratchpads
             , placeHook (inBounds (underMouse (0.5, 0.5)))
             ]

raiseNew :: ManageHook
raiseNew = do
  w <- ask
  doF $ \ws ->
    if (M.member w $ W.floating ws)
    then W.shiftMaster $ W.focusWindow w ws
    else ws

restartXMonad = spawn $ "kitty --name xmonadrestart -e /home/zubin/.xmonad/restart.sh"

myKeyBindings = concat
    [ xmonadControlBindings
    , appLaunchBindings
    , windowBindings
    , mediaBindings
    ]

setMode m = do
  setTo m
  updateMode

regularMode c = Mode "normal" GrabBound $ M.union additions (keys c c)
  where
    additions = mkKeymap c $
      [("M-<Esc>", setMode (navMode c))
      ,("M-S-<Esc>", setMode (minimalMode c))
      ]

minimalMode c = Mode "%{F#f40 R}minimal%{R F-}" GrabBound $ additions
  where
    additions = mkKeymap c $
      [("<Esc>", setMode (regularMode c))
      ,("M-<Esc>", setMode (regularMode c))
      ]

navMode c = Mode "%{F#fa0 R}nav%{R F-}" GrabBound $ additions
  where
    additions = mkKeymap c $ concat
      [ windowKeys
      , xmonadControlKeys
      , [("<Esc>", setMode (regularMode c))
        ,("M-<Esc>", setMode (regularMode c))
        ,("i", setMode (regularMode c))
        ]
      ]

appLaunchBindings =
    [("M-S-t", spawnHere "kitty")
    ,("M-<Return>", spawnHere "kitty")
    ,("M-z", spawnHere "kitty")
    ,("M-S-b", spawnHere "firefox")
    ,("M-S-k", spawn "xkill")
    ,("M-S-e", spawnHere "emacsclient -c")
    ,("M-g", scratchpadSpawnActionCustom "cd ~; kitty --name scratchpad")
    ,("<Insert>", pasteSelection )
    ,("<F10>", namedScratchpadAction scratchpads "ncmpcpp")
    ,("M-<F11>", namedScratchpadAction scratchpads "pavucontrol")
    ,("M-<F12>", namedScratchpadAction scratchpads "wicd")
    ,("M-q", restartXMonad)
    ,("M-r", shellPrompt myXPConfig)
    ,("M-v", spawn $ runInTerm' "~/scripts/playvid.sh")
    ,("M-S-g", spawn $ runInTerm "wget $(xsel --output --clipboard); read")
    ,("M-S-p", spawn "rofi-pass")
    ]

xmonadControlBindings = addSuperPrefix xmonadControlKeys

xmonadControlKeys =
    [("n", moveTo Next NonEmptyWS)
    ,("p", moveTo Prev NonEmptyWS)
    ,("`", toggleWS' ["NSP"])
    ,("S-x", kill1)
    ,("S-r", refresh)
    ,("x", killCopy)
    ,("C-d", sendMessage $ SPACING $ negate 5)
    ,("C-i", sendMessage $ SPACING 5)
    ,("C-S-d", removeWorkspace >> saveProjectState)
    ,(";", switchProjectPrompt myXPConfig >> saveProjectState)
    ,("d", changeProjectDirPrompt myXPConfig >> saveProjectState)
    ,("w", shiftToProjectPrompt myXPConfig)
    ,("e", gridselectWorkspace def W.view)
    ,("f", sendMessage $ Toggle NBFULL)
    ,("/", windowPrompt highlightConfig Goto allWindows)
    ,("S-/", windowPrompt highlightConfig Bring allWindows)
    ,("\\", windowMultiPrompt highlightConfig [(BringCopy,allWindows),(Bring,allWindows)])
    ,("<Space>", switchLayer)
    ,("S-<Space>", sendMessage NextLayout)
    ,("'", markFocused)
    ,("a", mergeMarked)
    ,("c", spawn "roficlip")
    ,("S-a", unmergeFocused)
    ,("[", onGroup W.focusUp')
    ,("]", onGroup W.focusDown')
    ,(".", sendMessage $ IncMasterN 1)
    ,(",", sendMessage $ IncMasterN (-1))
    ,("C-l", sendMessage Expand)
    ,("C-h", sendMessage Shrink)
    ,("C-k", sendMessage MirrorExpand)
    ,("C-j", sendMessage MirrorShrink)
    ]

windowBindings = addSuperPrefix windowKeys

windowKeys = do
  (key,dir) <- zip "hjkl" [L,D,U,R]
  id [([key], windowGo   dir False)
     ,("S-"++[key], windowSwap dir False)
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

myTabTheme = def { activeColor = "#cccccc"
                 , activeBorderColor = "#cccccc"
                 , activeTextColor = "#2b2b2b"
                 , inactiveColor = "#2b2b2b"
                 , inactiveBorderColor = "#2b2b2b"
                 , fontName = "xft:Source Code Pro-10"
                 }
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

readDynamicProjects :: [Project] -> IO [Project]
readDynamicProjects staticPs = do
  hndl <- openFile "/home/zubin/.xprojects" ReadWriteMode
  ls <- lines <$> hGetContents hndl
  let
    staticMap = M.fromList [(projectName p, p) | p <- staticPs]
    ps = do
      l <- ls
      ((name,dir),"") <- reads l
      return $ case M.lookup name staticMap of
        Nothing -> Project name dir Nothing
        Just p -> Project name dir (projectStartHook p)
    finalMap = foldr (\p -> M.insert (projectName p) p) staticMap ps

  return $ M.elems finalMap

saveProjectState :: X ()
saveProjectState = do
  ps <- allProjects
  io $ writeFile "/home/zubin/.xprojects" $ unlines $ do
    Project name dir _ <- ps
    return $ show (name,dir)

allWorkspaces :: X [String]
allWorkspaces = do
  ws <- gets (W.workspaces . windowset)
  return $ map W.tag ws

allProjects :: X [Project]
allProjects = do
  w <- allWorkspaces
  catMaybes <$> traverse lookupProject w

getFocused :: X (Maybe Window)
getFocused = withWindowSet (return . W.peek)

unmergeFocused :: X ()
unmergeFocused = do
  mw <- getFocused
  case mw of
    Nothing -> return ()
    Just x -> sendMessage $ UnMerge x

newtype MarkedWindow = MW { unMW :: Maybe Window }
instance ExtensionClass MarkedWindow where
  initialValue = MW Nothing

markFocused :: X ()
markFocused = getFocused >>= ES.put . MW

getMarked :: X (Maybe Window)
getMarked = unMW <$> ES.get

unmark :: X ()
unmark = ES.put $ MW Nothing

mergeMarked :: X ()
mergeMarked = do
  cur <- getFocused
  marked <- getMarked
  case (cur,marked) of
    (Just c, Just w) -> do
      sendMessage $ Merge w c
      unmark
    _ -> return ()
