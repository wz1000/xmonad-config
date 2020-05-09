{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import XMonad
import XMonad -- hiding ((|||))
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS ( toggleWS')
import XMonad.Actions.CopyWindow
import XMonad.Actions.Navigation2D ( withNavigation2DConfig
                                   , centerNavigation
                                   , sideNavigation
                                   , hybridOf
                                   , Navigation2DConfig(..)
                                   , windowGo
                                   , windowSwap
                                   , switchLayer)
import XMonad.Actions.SpawnOn
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
-- import XMonad.Actions.UpdateFocus
import XMonad.Layout.Fullscreen ( fullscreenFloat, FullscreenMessage(..) )
import XMonad.Layout.Simplest
import XMonad.Layout.Maximize
-- import XMonad.Layout.LayoutCombinators
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName   ( setWMName )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
import XMonad.Hooks.ToggleHook
import XMonad.Layout.BorderResize ( borderResize )
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.LayoutHints
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.StateFull
import XMonad.Layout.ResizableTileSub
import XMonad.Layout.NoBorders  ( smartBorders )
import XMonad.Prompt.Shell  ( shellPrompt )
import XMonad.Prompt.Window
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Run ( safeSpawn, runProcessWithInput )
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Invisible
import qualified XMonad.Util.ExtensibleState as ES
import XMonad.Util.Paste

import Spacing
import Modal hiding (defaultMode, normalMode, insertMode)
import qualified Modal as M

import Control.Monad
import Control.Monad.Extra ( mapMaybeM, whenM )
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.Maybe
import Data.Monoid
import System.IO
import System.Posix.Types (ProcessID)
import System.FilePath
import System.Directory

import Control.DeepSeq

startupApps = ["dunst"
              ,"pkill picom; exec picom"
              ,"feh --randomize --bg-fill ~/.wallpapers"
              ,"pkill polybar; exec polybar example"
              ]

runInTerm t x = myTerm ++ " --title " ++ t ++ " -e sh -c '" ++ x ++ "'"
mpvRunner x = myTerm ++ " --title mpvslave --class mpvslave -e sh -c '" ++ x ++ "'"

sshCommand = runInTerm "weechat"
  "TERM=xterm-256color mosh --ssh=\"ssh -i ~/.ssh/hetzner-irc\" $(cat ~/.ssh/irchost) tmux attach"

staticProjects :: [Project]
staticProjects =
  [ Project "main" "~" Nothing
  , Project "web" "~" (Just $ spawnOn "web" "firefox")
  , Project "mail" "~" (Just $ spawnOn "mail" $ runInTerm "neomutt" "neomutt")
  , Project "irc" "~" (Just $ spawnOn "irc" sshCommand)
  , Project "dev" "~" Nothing
  , Project "conf" "~" Nothing
  , Project "play" "~" Nothing
  , Project "media" "~" Nothing
  ]

myTerm = "kitty -1"
-- myTerm = "alacritty"

myEwmh :: XConfig a -> XConfig a
myEwmh c =
  c { startupHook     = startupHook c +++ ewmhDesktopsStartup
    , handleEventHook = handleEventHook c +++ ewmhDesktopsEventHookCustom customFilter
    , logHook         = logHook c +++ ewmhDesktopsLogHookCustom customFilter
    }
 where
   x +++ y = mappend y x
   customFilter = filter $ (/= "NSP") . W.tag

main :: IO ()
main = do
    ps <- readDynamicProjects staticProjects
    forM_ [".xmonad-mode-log"] $ \file -> do
      safeSpawn "mkfifo" ["/tmp/" ++ file]

    xmonad
     $ dynamicProjects ps
     $ withNavigation2DConfig ( def { defaultTiledNavigation = hybridOf sideNavigation centerNavigation } )
     $ docks
     $ modal regularMode
     $ myEwmh
     $ (def
        { terminal        = myTerm
        , borderWidth     = 2
        , focusedBorderColor = "#cccccc"
        , normalBorderColor = "#3c3c3c"
        , rootMask = rootMask def -- .|. pointerMotionMask
        , clientMask = clientMask def -- .|. pointerMotionMask
        , modMask         = mod4Mask
        , layoutHook      = focusTracking $ historyLayout myLayout
        , workspaces      = myWorkspaces ++ (map projectName ps L.\\ myWorkspaces)
        , manageHook      = myManageHook
        , handleEventHook = handleEventHook def <+> hintsEventHook
        , logHook         = updateMode <> colorMarked <> runAllPending
        , startupHook     = do
            setWMName "LG3D"
            mapM_ spawn startupApps
        } `additionalKeysP` myKeyBindings
          `additionalMouseBindings` myMouseBindings
          )-- `removeKeysP` [pref ++ [n] | pref <- ["M-S-","M-"], n <- ['1'..'9']])

myMouseBindings = []

isCurMirrored :: X Bool
isCurMirrored = do
  w <- gets (W.currentTag . windowset)
  ES.gets (M.findWithDefault False w . getMirrored)

newtype Mirrored = Mirrored { getMirrored :: M.Map WorkspaceId Bool } deriving (Read, Show, Typeable)
instance ExtensionClass Mirrored where
  initialValue = Mirrored mempty
  extensionType = PersistentExtension

mirrorLayout = do
  w <- gets (W.currentTag . windowset)
  sendMessage $ MT.Toggle MIRROR
  let invertor Nothing = Just True
      invertor (Just x) = Just (not x)
  ES.modify (Mirrored . M.alter invertor w . getMirrored)

resizeIn :: Direction2D -> X ()
resizeIn d = do
  m <- isCurMirrored
  case (d,m) of
    (L,False) -> sendMessage Shrink
    (R,False) -> sendMessage Expand
    (U,False) -> sendMessage MirrorExpand
    (D,False) -> sendMessage MirrorShrink
    (L, True) -> sendMessage MirrorExpand
    (R, True) -> sendMessage MirrorShrink
    (U, True) -> sendMessage Shrink
    (D, True) -> sendMessage Expand

newtype FocusHistory = FH { getFocusHistory :: [Window] } deriving (Read, Show, Typeable)
instance ExtensionClass FocusHistory where
  initialValue = FH []
  extensionType = PersistentExtension

data FocusLayout a = FL deriving (Read,Show,Typeable)

historyLayout :: l Window -> ModifiedLayout FocusLayout l Window
historyLayout = ModifiedLayout FL

instance LayoutModifier FocusLayout Window where
  modifyLayout fh ws rct = do
    wold <- getFocused
    hist <- getFocusHistory <$> ES.get
    wnew <- windowHistoryHook wold
    case wnew of
      Nothing -> runLayout ws rct
      Just w -> do
        let oldstack = W.stack ws
        let mw = L.find (`elem` W.integrate' oldstack) hist
        let newstack = if (w `elem` (W.integrate' oldstack))
                       then until ((w==) . W.focus) W.focusUp' <$> oldstack
                       else case mw of
                         Just w -> until ((w==) . W.focus) W.focusUp' <$> oldstack
                         Nothing -> oldstack
        modifyWindowSet (W.focusWindow w)
        addAction $ do
          maybe (return ()) makeBorderNormal wold
          windows id
        runLayout ws{W.stack = newstack} rct

windowHistoryHook Nothing = return Nothing
windowHistoryHook (Just w) = do
  hist <- getFocusHistory <$> ES.get
  curws <- gets $ W.index . windowset
  withWindowSet $ \allws ->
    case hist of
      [] -> do
        ES.put $ FH [w]
        return Nothing
      (prev:xs)
          | prev == w -> return Nothing
          -- Previous focus was removed from ws, focus on previous existing window in current ws
          | not (prev `elem` curws) -> do
              let hist' = filter (`W.member` allws) xs
              ES.put (FH $ force $ hist')
              return $ L.find (\x -> x `elem` curws ) hist'
          -- Add current focus to history
          | otherwise -> do
              ES.put $ FH $ force $ (w:L.delete w hist)
              return Nothing

raiseFloating w = do
  isFloat <- gets $ M.member w . W.floating . windowset
  if isFloat
  then windows $ (W.shiftMaster .) $ W.focusWindow w
  else return ()

updateMode = do
  mode <- ES.gets M.label
  willMerge <- willHookNext "merge"
  m <- isCurMirrored
  let layouticon = "%{T3}"++(if m then "\57354" else "\57353")++"%{T-}"
  let modeColor = case mode of
        "nav" -> "#fa0"
        "minimal" -> "#f40"
        _ -> "#cacaca"
  let prettyMode = "%{F"++modeColor ++ " R}\57532" ++ layouticon ++ " " ++ mode ++ "\57534%{RF-}"
  dir <- io . canonicalizePath =<< expandHome "/home/zubin" . projectDirectory <$> currentProject
  let reldir = makeRelative "/home/zubin" dir
      parents = joinPath $ map (take 2) $ splitPath $ takeDirectory reldir
      shortDir = dropTrailingPathSeparator
               $ normalise
               $ "~" </> replaceDirectory reldir parents
  let prettyDir = shortDir ++ " "
  io $ appendFile "/tmp/.xmonad-mode-log" prettyDir
  if willMerge
  then io $ appendFile "/tmp/.xmonad-mode-log" (prettyMode ++ " merge")
  else io $ appendFile "/tmp/.xmonad-mode-log" (prettyMode ++ "")
  io $ appendFile "/tmp/.xmonad-mode-log" "\n"

expandHome :: FilePath -> FilePath -> FilePath
expandHome home dir = case L.stripPrefix "~" dir of
  Nothing -> dir
  Just xs -> home ++ xs

colorMarked = getMarked >>= mapM_ makeBorderRed

myWorkspaces =
            ["main"
            ,"irc"
            ,"web"
            ,"mail"
            ,"dev"
            ,"conf"
            ,"play"
            ,"media"
            ]

instance Shrinker CustomShrink where
  shrinkIt s cs
    | "Kakoune" `L.isSuffixOf` cs = shrinkKak cs
    | otherwise = shrinkIt shrinkText cs

shrinkKak cs = cs : cs' : cs'' : shortDir : L.tails shortDir
  where
    words = L.splitOn " - " cs
    cs'  = L.intercalate " - " $ L.dropEnd 1 words
    cs'' = L.intercalate " - " $ L.dropEnd 2 words
    fs = L.splitOn "/" cs''
    shortDir = case fs of
      [] -> []
      xs -> L.intercalate "/" $ map (take 2) (init fs) ++ [last fs]

myLayout = smartBorders
         $ borderResize
         $ fullscreenFloat
         $ toggleLayouts (StateFull)
         $ avoidStruts
         $ maximizeWithPadding 20
         $ layoutHintsWithPlacement (0.5,0.5)
         $ layouts
          where
              layouts = addTabs CustomShrink myTabTheme
                $ subLayout [] Simplest $ mkToggle1 MIRROR myTall
              myTall = spacing 0 $ resizable 40 5
              resizable step n = ResizableTall 1 (1/step) ((1/2)+n/step) [] (I [])
              -- ^ n is no of step-lengths right of center, which is used as the
              -- default split ratio
kittyPopup = "kitty -o background_opacity=0.95"
scratchpads =
  [ NS "pavucontrol" "pavucontrol" (resource =? "pavucontrol") defaultFloating
  , NS "ncmpcpp" (kittyPopup++" --class ncmpcpp -e ncmpcpp") (resource =? "ncmpcpp") defaultFloating
  , NS "htop" (kittyPopup++" --class htop -e htop") (resource =? "htop") defaultFloating
  , NS "ytop" (kittyPopup++" --class ytop -e ytop -p") (resource =? "ytop") defaultFloating
  , NS "battop" (kittyPopup++" --class battop -e battop") (resource =? "battop") defaultFloating
  , NS "bandwhich" (kittyPopup++" --class bandwhich -e bandwhich") (resource =? "bandwhich") defaultFloating
  , NS "wicd" "wicd-client --no-tray" (resource =? "wicd-client.py") defaultFloating
  , NS "clerk" (kittyPopup++" --class clerk -e clerk") (className =? "clerk") defaultFloating
  , NS "calcurse" (kittyPopup++" --class calcurse -e calcurse -q") (className =? "calcurse") defaultFloating
  , NS "dynamic" ("notify-send 'No dynamic scratchpad!'") dynamicScratchpadQuery defaultFloating
  ]

newtype DynamicScratchpad
  = DS { getDynamicScratchpad :: Maybe Window }
  deriving (Eq,Read,Show)
instance ExtensionClass DynamicScratchpad where
  initialValue = DS Nothing
  extensionType = PersistentExtension

dynamicScratchpadQuery = do
  w <- ask
  liftX $ ES.gets ((Just w ==) . getDynamicScratchpad)

makeDynamicScratchpad :: X ()
makeDynamicScratchpad = do
  ES.put . DS =<< logTitle =<< getFocused
  where
    logTitle Nothing = do
      spawn "notify-send 'Removing dynamic scratchpad!'"
      pure Nothing
    logTitle (Just w) = do
      n <- show <$> getName w
      spawn $ "notify-send 'Adding dynamic scratchpad!' '" ++ n ++ "'"
      pure (Just w)

dynamicScratchpadAction :: X ()
dynamicScratchpadAction = do
  namedScratchpadAction scratchpads "dynamic"
  mw <- getFocused
  forM_ mw $ \w ->
    whenM (runQuery dynamicScratchpadQuery w) $
      windows W.swapDown

combineMPV :: X ()
combineMPV = withWindowSet $ \s -> do
  let winList = maybe [] W.integrate . W.stack . W.workspace . W.current $ s
  slaves <- filterM (runQuery $ resource =? "mpvslave" ) winList
  masters <- filterM (runQuery $ resource =? "mpvytdl" ) winList
  case (slaves,masters) of
    ((slave:_),(master:_)) -> do
      sendMessage $ Merge slave master
    _ -> return ()

markMpvMerge :: Monoid m => Query m
markMpvMerge = do
  liftX $ addAction combineMPV
  return mempty

mergeIntoFocused = mergeIntoFocusedIf (pure True)

mergeIntoFocusedIf q = do
  w <- ask
  liftX $ addAction $ do
    xs <- ES.gets getFocusHistory
    curws <- gets $ W.index . windowset
    case L.find (liftM2 (&&) (/= w) (`elem` curws)) xs of
      Nothing -> return ()
      Just x ->
        whenM (runQuery q x) $
          sendMessage $ Migrate w x
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
    , resource =? "mpvytdl"            --> markMpvMerge
    , resource =? "pavucontrol"        --> placeHook ( fixed (1,25/1080) ) <+> doFloat
    , resource =? "wicd-client.py"     --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "gsimplecal"         --> placeHook ( fixed (1,35/1080) )
    , resource =? "alsamixer"          --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "nethogs"            --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "progress"           --> placeHook ( fixed (1,35/1080) ) <+> doFloat
    , resource =? "runner"             --> placeHook ( fixed (0,1) ) <+> doFloat
    , resource =? "feh"                --> doIgnore
    , resource =? "dzen2"              --> doIgnore
    , resource =? "polybar"            --> doIgnore
    , resource =? "scratchpad"         --> doRectFloat (centerAligned 0.5 0.3 0.45 0.45)
    , resource =? "htop"               --> doRectFloat (centerAligned 0.75 (15/1080) 0.5 0.65)
    , resource =? "ytop"               --> doRectFloat (centerAligned 0.75 (15/1080) 0.5 0.8)
    , resource =? "battop"             --> doRectFloat (centerAligned 0.75 (15/1080) 0.5 0.6)
    , resource =? "calcurse"           --> doRectFloat (centerAligned 0.80 (15/1080) 0.40 0.5)
    , resource =? "bandwhich"          --> doRectFloat (W.RationalRect 0.42 (15/1080) 0.58 0.6)
    , resource =? "unicodeinp"         --> doRectFloat (centerAligned 0.5 0.3 0.45 0.45)
    , resource =? "kittypopup"         --> doRectFloat (centerAligned 0.5 0.2 0.55 0.65)
    , resource =? "xmonadrestart"      --> doRectFloat (centerAligned 0.5 0.3 0.35 0.35)
    , resource =? "ncmpcpp"            --> doRectFloat (centerAligned 0.5 (15/1080) (2/3) 0.6)
    , resource =? "clerk"              --> placeHook ( fixed (0.5,55/1080) ) <+> doFloat
    , resource =? "org.pwmt.zathura"   --> mergeIntoFocusedIf (not <$> className =? "firefox")
                                        <* liftX (addAction $ saveWindows False)
    , className =? "mpv"               --> pure mempty      <* liftX (addAction $ saveWindows False)
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
             , placeHook (smart (1/2,1/2))
             , toggleHook "merge" mergeIntoFocused
             ]

raiseNew :: ManageHook
raiseNew = do
  w <- ask
  doF $ \ws ->
    if (M.member w $ W.floating ws)
    then W.shiftMaster $ W.focusWindow w ws
    else ws

restartXMonad = do
  broadcastMessage ReleaseResources
  io . flush =<< asks display
  writeStateToFile
  spawn (kittyPopup++" --class xmonadrestart -e /home/zubin/.xmonad/restart-cabal.sh")

myKeyBindings = concat
    [ xmonadControlBindings
    , appLaunchBindings
    , windowBindings
    , mediaBindings
    ] ++ [("M-M1-<F"++n++">", spawn ("chvt " ++ n)) | i <- [1..7], let n = show i]

setMode m = do
  setTo m
  updateMode

regularMode c = Mode "normal" GrabBound $ M.union additions (keys c c)
  where
    additions = mkKeymap c $
      [("M-<Esc>", setMode (navMode c))
      ,("M-S-<Esc>", setMode (minimalMode c))
      ]

minimalMode c = Mode "minimal" GrabBound $ additions
  where
    additions = mkKeymap c $
      [("<Esc>", setMode (regularMode c))
      ,("M-<Esc>", setMode (regularMode c))
      ]

navMode c = Mode "nav" GrabBound $ additions
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
    [("M-S-t", hookNext "merge" True >> spawnHere myTerm)
    ,("M-<Return>", spawnHere myTerm)
    ,("M-S-b", spawnHere "firefox")
    ,("M-S-e", hookNext "merge" True >> spawnHere "kitty -e zsh -c 'source ~/.zsh_funcs && e'")
    ,("M-u", spawn "kitty --class unicodeinp -o background_opacity=0.90 -e sh -c '(kitty +kitten unicode_input | tr -d \"\\n\"| xsel)' && xdotool click 2")
    ,("M-S-f", spawnHere $ runInTerm "ranger" "ranger")
    ,("M-g", scratchpadSpawnActionCustom $ unwords ["cd ~;",kittyPopup,"--class scratchpad -e ~/scripts/detachable"])
    ,("<Insert>", pasteSelection)
    ,("M-<F6>", namedScratchpadAction scratchpads "calcurse")
    ,("M-<F7>", namedScratchpadAction scratchpads "htop")
    ,("M-<F8>", namedScratchpadAction scratchpads "bandwhich")
    ,("M-<F9>", namedScratchpadAction scratchpads "ytop")
    ,("M-<F10>", namedScratchpadAction scratchpads "battop")
    ,("<F10>", namedScratchpadAction scratchpads "ncmpcpp")
    ,("M-<F11>", namedScratchpadAction scratchpads "pavucontrol")
    ,("M-<F12>", namedScratchpadAction scratchpads "wicd")
    ,("M-i", dynamicScratchpadAction)
    ,("M-S-i", makeDynamicScratchpad)
    ,("M-q", restartXMonad)
    ,("M-r", shellPrompt myXPConfig)
    ,("M-v", spawn $ mpvRunner "~/scripts/playvid.sh")
    ,("M-S-=", spawn $ "~/scripts/html.sh")
    ,("M-S-g", spawn $ runInTerm "aria2c" "~/scripts/download.sh $(xsel --output --clipboard)")
    ,("M-C-p", spawn "rofi-pass")
    ,("M-S-s", saveWindows True)
    ,("M-S-r", restoreWindows)
    ,("<Print>", spawn "maim -u ~/$(date '+%Y-%m-%d-%H%m%S_grab.png')")
    ,("S-<Print>", spawn "maim -us ~/$(date '+%Y-%m-%d-%H%m%S_grab.png')")
    ,("M-<Print>", spawn "maim -ui $(xdotool getactivewindow) ~/$(date '+%Y-%m-%d-%H%m%S_grab.png')")
    ]

makeBorderRed :: Window -> X ()
makeBorderRed w =
    withDisplay $ \d -> io $ do
      setWindowBorder d w 0xff0000

makeBorderNormal w =
    withDisplay $ \d -> io $ do
      setWindowBorder d w 0x3c3c3c

makeBorderFocused w =
    withDisplay $ \d -> io $ do
      setWindowBorder d w 0xcccccc

floatFull = do
  mw <- getFocused
  case mw of
    Nothing -> pure ()
    Just w -> do
      floating <- gets $ W.floating . windowset
      let mr = M.lookup w floating
      case mr of
        Nothing -> sendMessage ToggleLayout
        Just r
          | r == W.RationalRect 0 0 1 1 -> do
              broadcastMessage $ RemoveFullscreen w
              sendMessage FullscreenChanged
          | otherwise -> do
              broadcastMessage $ AddFullscreen w
              sendMessage FullscreenChanged

cylceOptions w = map (W.view `flip` w) (recentTags w)
  where
    recentTags w = filter (/= "NSP") $ map W.tag $ tail (W.workspaces w) ++ [head (W.workspaces w)]

xmonadControlBindings = addSuperPrefix xmonadControlKeys

xmonadControlKeys =
    [("n", windows W.focusDown)
    ,("p", windows W.focusUp)
    ,("y", withFocused (sendMessage . maximizeRestore))
    ,("S-n", windows W.swapDown)
    ,("S-p", windows W.swapUp)
    ,("`", toggleWS' ["NSP"])
    ,("S-x", kill1)
    ,("<Tab>",cycleWindowSets cylceOptions [xK_Super_L] xK_Tab xK_grave)
    ,("C-<Return>", dwmpromote )
    ,("S-m", toggleHookNext "merge" >> updateMode )
    ,("C-r", refresh)
    ,("x", killCopy)
    ,("C-d", sendMessage $ SPACING $ negate 5)
    ,("C-i", sendMessage $ SPACING 5)
    ,("b", sendMessage ToggleStruts)
    ,("z", mirrorLayout >> updateMode )
    ,("C-S-d", removeWorkspace >> saveProjectState)
    ,(";", switchProjectPrompt myXPConfig >> saveProjectState)
    ,("d", changeProjectDirPrompt myXPConfig >> saveProjectState >> updateMode)
    ,("w", shiftToProjectPrompt myXPConfig)
    ,("e", dwmpromote )
    ,("f", floatFull)
    ,("/", windowPrompt highlightConfig Goto allWindows)
    ,("C-/", tabPrompt)
    ,("S-/", windowMultiPrompt highlightConfig [(bringAsTabbed,allWindows),(Bring,allWindows)])
    ,("\\" , windowMultiPrompt highlightConfig [(bringCopyAsTabbed, allWindows),(BringCopy,allWindows)])
    ,("<Space>", switchLayer)
    ,("S-<Space>", sendMessage NextLayout)
    ,("C-S-<Space>", sendMessage FirstLayout)
    ,("'", markFocused)
    ,("a", mergeMarked)
    ,("s", swapWithMarked)
    ,("o", shiftMarked)
    ,("c", spawn "roficlip")
    ,("S-a", unmergeFocused)
    ,("[", onGroup W.focusUp')
    ,("]", onGroup W.focusDown')
    ,("S-[", onGroup swapUp')
    ,("S-]", onGroup swapDown')
    ,(".", sendMessage $ IncMasterN 1)
    ,(",", sendMessage $ IncMasterN (-1))
    ,("C-l", resizeIn R)
    ,("C-h", resizeIn L)
    ,("C-k", resizeIn U)
    ,("C-j", resizeIn D)
    ]

bringCopyAsTabbed = WithWindow "Bring copy to tab group: " $ \w -> do
  cur <- getFocused
  windows $ \ws -> copyWindow w (W.currentTag ws) ws
  maybe (pure ()) (sendMessage . Migrate w) cur
  windows W.swapDown

bringAsTabbed = WithWindow "Bring to tab group: " $ \w -> do
  cur <- getFocused
  windows $ \ws -> W.shiftWin (W.currentTag ws) w ws
  maybe (pure ()) (sendMessage . Migrate w) cur
  windows W.swapDown

tabPrompt :: X ()
tabPrompt = do
  mw <- getFocused
  case mw of
    Nothing -> return ()
    Just w -> sendMessage $ WithGroup go w
  where
    go st = do
      let xs = W.integrate st
          winmap = fmap M.fromList $ forM xs $ \x -> do
            name <- show <$> getName x
            return (name,x)
      windowPrompt highlightConfig Goto winmap

      return st

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
    ,("<XF86AudioStop>", spawn "mpc stop")
    ,("<XF86AudioMute>", spawn "~/scripts/dvol2 -t")
    ,("<XF86AudioLowerVolume>", spawn "~/scripts/dvol2 -d 2")
    ,("<XF86AudioRaiseVolume>", spawn "~/scripts/dvol2 -i 2")
    ,("<XF86MonBrightnessUp>", spawn "light -A 5")
    ,("<XF86MonBrightnessDown>", spawn "light -U 5")
    ,("M-C-m", spawn "clerk -t")
    ,("M-m n", spawn "mpc next")
    ,("M-m p", spawn "mpc prev")
    ,("M-m <Space>", spawn "mpc toggle")
    ]

myTabTheme = def { activeColor = "#cccccc"
                 , activeBorderColor = "#cccccc"
                 , activeTextColor = "#2b2b2b"
                 , inactiveColor = "#2b2b2b"
                 , inactiveBorderColor = "#3c3c3c"
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

saveQuery :: Query (Maybe ProcessID)
saveQuery = do
  willSave <- resource =? "org.pwmt.zathura" <||> className =? "mpv"
  if willSave
  then pid
  else return Nothing

saveWindows notify = do
  curws <- gets $ W.index . windowset
  cmds <- flip mapMaybeM curws $ \w -> do
    mpid <- runQuery saveQuery w
    case mpid of
      Nothing -> return Nothing
      Just pid -> do
        cmd <- runProcessWithInput "/home/zubin/scripts/getcommand.sh" [show pid] ""
        return $ Just cmd
  Project name _ _ <- currentProject
  io $ writeFile ("/home/zubin/.cache/xmonad-resume/"++name) $ show cmds
  when notify $ spawn $ "notify-send Saved!"

restoreWindows = do
  Project name _ _ <- currentProject
  cmds <- liftIO $
    withFile ("/home/zubin/.cache/xmonad-resume/"++name)
             ReadMode $ \hndl -> do
      cmds <- read <$> hGetContents hndl
      return $!! (cmds :: [String])
  mapM_ spawn cmds
  spawn $ "notify-send Restored!"

killCopy :: X ()
killCopy = do
    ss <- gets windowset
    whenJust (W.peek ss) $
      \w -> if W.member w $ delete'' w ss
            then windows $ delete'' w
            else return ()
  where delete'' w = W.modify Nothing (W.filter (/= w))

readDynamicProjects :: [Project] -> IO [Project]
readDynamicProjects staticPs =
  withFile "/home/zubin/.xprojects" ReadMode $ \hndl -> do
  ls <- lines <$> hGetContents hndl
  let
    staticMap = M.fromList [(projectName p, p) | p <- staticPs]
    ps = do
      l <- ls
      ((name,dir),"") <- reads l
      return $ case M.lookup name staticMap of
        Nothing -> Project name dir (Just $ pure ())
        Just p -> Project name dir (projectStartHook p)
    finalMap = foldr (\p -> M.insert (projectName p) p) staticMap ps

  return $! M.elems finalMap

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

reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls

swapDown' :: W.Stack a -> W.Stack a
swapDown'  = reverseStack . swapUp' . reverseStack

swapUp' :: W.Stack a -> W.Stack a
swapUp' (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp' (W.Stack t []     rs) = W.Stack t (reverse rs) []

newtype MarkedWindow = MW { unMW :: Maybe Window }
instance ExtensionClass MarkedWindow where
  initialValue = MW Nothing

markFocused :: X ()
markFocused = do
  cur <- getFocused
  MW old <- ES.get
  if cur == old
  then do
    ES.put $ MW Nothing
    mapM_ makeBorderFocused cur
  else do
    ES.put $ MW cur
    mapM_ makeBorderNormal old
    mapM_ makeBorderRed cur

getMarked :: X (Maybe Window)
getMarked = do
  mw <- unMW <$> ES.get
  ws <- gets $ W.allWindows . windowset
  case mw of
    Nothing -> return Nothing
    Just w
      | w `elem` ws -> return mw
      | otherwise -> do
        ES.put $ MW Nothing
        return Nothing

unmark :: X ()
unmark = ES.put $ MW Nothing

mergeMarked :: X ()
mergeMarked = do
  cur <- getFocused
  marked <- getMarked
  case (cur,marked) of
    (Just c, Just w) -> do
      sendMessage $ Migrate w c
      unmark
      windows $ W.focusWindow w
      makeBorderFocused w
    _ -> return ()

swapWithMarked :: X ()
swapWithMarked = do
  cur <- getFocused
  marked <- getMarked
  case (cur,marked) of
    (Just c, Just w) -> do
      swapWindows c w
      makeBorderFocused w
      unmark
    _ -> return ()

swapWindows :: Window -> Window -> X ()
swapWindows a b = windows $ W.mapWorkspace swapWorkspace
  where
    swapWorkspace ws = ws{W.stack = swapped <$> W.stack ws}
    swapped (W.Stack x ls rs) = W.Stack (swap x) (swap <$> ls) (swap <$> rs)
    swap x
      | x == a = b
      | x == b = a
      | otherwise = x

shiftMarked :: X ()
shiftMarked = do
  marked <- getMarked
  case marked of
    Just c -> do
      windows $ \ss -> W.shiftWin (W.currentTag ss) c ss
      makeBorderFocused c
      unmark
    _ -> return ()
