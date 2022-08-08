{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Layout.Fullscreen ( fullscreenFloat, FullscreenMessage(..), fullscreenFocus )
import XMonad.Layout.Simplest
import XMonad.Layout.Maximize
-- import XMonad.Layout.LayoutCombinators
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
import XMonad.Hooks.ToggleHook
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.LayoutHints
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Column
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.StateFull
import XMonad.Layout.ResizableTileSub
import XMonad.Layout.NoBorders
import XMonad.Prompt.Zsh  ( zshPrompt )
import XMonad.Prompt.Window
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Hacks
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
import Data.Coerce
import Data.Maybe
import System.IO
import System.Posix.Types (ProcessID)
import System.FilePath
import System.Directory
import Text.Read
import GHC.ResponseFile
import XMonad.Util.XSelection

import Control.DeepSeq

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import Data.Functor
import Data.Semigroup

-- import MyBar
-- import Xmobar (xmobar)
-- import Control.Concurrent
-- import Control.Concurrent.STM
import Data.IORef

import XMonad.Util.Loggers
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import KeyDesc
import XMonad.Actions.Submap
import XMonad.Util.NamedActions
import XMonad.Util.XUtils

startupApps = ["exec dunst"
              ,"exec picom"
              ,"feh --randomize --bg-fill ~/.wallpapers"
              ]

runInTerm mt x = do
  pure $ myTerm ++ (case mt of Just title -> " --title " ++ title; Nothing -> "") ++ " -e sh -c '" ++ x ++ "'"
sshCommand = runInTerm (Just "irc") "TERM=xterm-256color mosh --ssh=\"ssh -i ~/.ssh/hetzner-irc\" $(cat ~/.ssh/irchost) tmux attach"

staticProjects :: [Project]
staticProjects =
  [ Project "main" "~" Nothing
  , Project "web" "~" (Just $ spawnOn "web" "firefox")
  , Project "irc" "~" (Just $ spawnOn "irc" =<< sshCommand)
  , Project "dev" "~" Nothing
  , Project "conf" "~" Nothing
  , Project "play" "~" Nothing
  , Project "media" "~" Nothing
  ]

myTerm = "kitty"
-- myTerm = "alacritty"

myEwmh :: XConfig a -> XConfig a
myEwmh c =
  c { startupHook     = ewmhDesktopsStartup <> startupHook c
    , handleEventHook = ewmhDesktopsEventHookCustom customFilter <> handleEventHook c
    , logHook         = ewmhDesktopsLogHookCustom customFilter <> logHook c
    }
 where
   customFilter = filter $ (/= "NSP") . W.tag

main :: IO ()
main = do
    ps <- readDynamicProjects staticProjects
    forM_ [".xmonad-mode-log"] $ \file -> do
      let fifo = "/tmp/"++file
      whenM (io $ not <$> doesFileExist fifo) $
        void $ runProcessWithInput "mkfifo" [fifo] ""
    xmonad $ myConfig ps

myConfig ps
  = dynamicProjects ps
  -- $ withUrgencyHookC myUrgencyHook urgencyConfig{ suppressWhen = Focused }
  $ withNavigation2DConfig ( def { defaultTiledNavigation = centerNavigation } )
  $ docks
  $ dynamicSBs myBarConfigs
  $ modal regularMode
  $ myEwmh
  $ def
  { terminal           = myTerm
  , borderWidth        = 2
  , focusedBorderColor = "#cccccc"
  , normalBorderColor  = "#3c3c3c"
  , rootMask           = rootMask def -- .|. pointerMotionMask
  , clientMask         = clientMask def -- .|. pointerMotionMask
  , modMask            = mod4Mask
  , layoutHook         = focusTracking $ historyLayout $ myLayout
  , workspaces         = myWorkspaces ++ (map projectName ps L.\\ myWorkspaces)
  , manageHook         = myManageHook
  , handleEventHook    = mconcat
                       [ handleEventHook def
                       , hintsEventHook
                       , serverModeEventHookF "XMONAD_COMMAND" (myServer . unescapeArgs)
                       , windowedFullscreenFixEventHook
                       ]
  , logHook            = colorMarked <> runAllPending <> updatePointer (0.5, 0.5) (0.9, 0.9)
  , startupHook        = mapM_ spawn startupApps >> checkKeymap (myConfig ps) myKeyBindings >> setProjectDir >> toggleScreens
  } `additionalKeysP` myKeyBindings
    `removeMouseBindings` (map fst myMouseBindings)
    `additionalMouseBindings` myMouseBindings
          -- )`removeKeysP` [pref ++ [n] | pref <- ["M-S-","M-"], n <- ['1'..'9']])

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings = [((mod4Mask,4), const $ spawn "~/scripts/dvol2 -i 2" )
                  ,((mod4Mask,5), const $ spawn "~/scripts/dvol2 -d 2" )
                  ,((mod4Mask,2), mouseSwap)
                  ]

mouseSwap w1 = do
  pos_r <- liftIO $ newIORef Nothing
  mouseDrag (\x y -> liftIO $ writeIORef pos_r $ Just (x,y)) $ do
    liftIO (readIORef pos_r) >>= \case
      Nothing -> pure ()
      Just (x,y) -> do
        rects <- getAllRectangles
        whenJust (L.find (\(rect,_) -> pointWithin x y rect) rects) $ \(_, w2) ->
          swapWindows w1 w2

getAllRectangles = do ws <- gets windowset
                      let allWindows = join $ map (W.integrate' . W.stack)
                                         $ (W.workspace . W.current) ws
                                         : (map W.workspace . W.visible) ws
                                         ++ W.hidden ws
                      allRects <- mapM getWindowRectangle allWindows
                      return $ zip allRects allWindows

getWindowRectangle :: Window -> X Rectangle
getWindowRectangle window
  = do d <- asks display
       (_, x, y, w, h, _, _) <- io $ getGeometry d window

         -- We can't use the border width returned by
         -- getGeometry because it will be 0 if the
         -- window isn't mapped yet.
       b <- asks $ borderWidth . config
       return $ Rectangle x y (w + 2*b) (h + 2*b)

myBarConfigs :: ScreenId -> IO StatusBarConfig
myBarConfigs n@(S i) = do
  let polybarOnMSI
        | n == 0    = statusBarGeneric "polybar mybar" (pure ())
        | otherwise = mempty
  xmobar <- statusBarPipe ("mybar " ++ show i) (pure $ xmobarForScreen n)
  -- logQueue <- newTQueueIO
  -- tid <- forkIO (xmobar (barConfig i logQueue))
  pure $ polybarOnMSI <> xmobar {-StatusBarConfig
      { sbLogHook = liftIO . atomically . writeTQueue logQueue =<< dynamicLogString (xmobarForScreen n)
      , sbStartupHook = pure ()
      , sbCleanupHook = liftIO $ do
          killThread tid
          pure ()
      }-}

xmobarForScreen :: ScreenId -> PP
xmobarForScreen screen
  = def
  { ppCurrent = nerdBordersRound 
               -- ^ how to print the tag of the currently focused
               -- workspace
  , ppVisible = xmobarColor "#908e92" ""
    -- ^ how to print tags of visible but not focused
    -- workspaces (xinerama only)
  , ppHidden  = id
    -- ^ how to print tags of hidden workspaces which
    -- contain windows
  , ppHiddenNoWindows = const ""
    -- ^ how to print tags of empty hidden workspaces
  , ppVisibleNoWindows = Nothing
    -- ^ how to print tags of empty visible workspaces
  , ppUrgent = xmobarColor "yellow" ""
    -- ^ format to be applied to tags of urgent workspaces.
  , ppRename = \s ws -> xmobarAction ("xmonadctl full " ++ W.tag ws) "3" $ xmobarAction ("xmonadctl project " ++ W.tag ws) "1" s
    -- ^ rename/augment the workspace tag
    --   (note that @WindowSpace -> …@ acts as a Reader monad)
  , ppSep = " | "
  , ppWsSep = " "
  , ppTitle = const ""
  , ppTitleSanitize = id
  , ppLayout = const ""
  , ppOrder = \(wss:_l:_t:curws:title:xs) -> [ xmobarColor "#2b2b2b" "#9acaca"
                                                $ xmobarAction "xmonadctl cyclews" "1"
                                                $ xmobarAction ("xmonadctl mono " ++ curws) "3" curws
                                             , scroll wss]
                                          ++ xs
                                          ++ [xmobarColor "#fa609f" "" title]
  , ppSort = do
      st <- getSortByIndex
      pure $ (filterOutWs ["NSP"] . st)
  , ppExtras = [logCurrentOnScreen screen,logTitleOnScreen screen, modeL, numL , dirL, mergeL, branchL]
  }
  where
    scroll = xmobarAction "xmonadctl workspace prev" "4" . xmobarAction "xmonadctl workspace next" "5"
    branchL = do
      Just ws <- logCurrentOnScreen screen
      Just proj <- lookupProject ws
      dir <- io . canonicalizePath $ expandHome "/home/zubin" $ projectDirectory proj
      branch <- L.trim <$> runProcessWithInput "git" ["-C",dir, "rev-parse","--abbrev-ref","HEAD"] ""
      let branchText
            | branch /= "master" = Just $ xmobarColor "#f0a0a0" "" branch
            | otherwise = Nothing
      pure branchText
    mergeL = do
      willMerge <- willHookNext "merge"
      pure $ if willMerge then Just "merge" else Nothing
    modeL = do
      mode <- ES.gets M.label
      m <- isCurMirrored
      let modeColor = case mode of
            "nav" -> "#f0a000"
            "minimal" -> "#f04000"
            _ -> "#cacaca"
      let layouticon = xmobarFont 1 $ if m then "\57354" else "\57353"
      let prettyMode
            = Just $ nerdBordersPoint modeColor (layouticon ++ mode)
      pure prettyMode
    numL = do
      ss <- withWindowSet $ return . W.screens
      case L.find ((== screen) . W.screen) ss of
        Just s  -> pure . fmap (show . length) . W.stack . W.workspace $ s
        Nothing -> pure Nothing
    dirL = do
      Just ws <- logCurrentOnScreen screen
      Just proj <- lookupProject ws
      dir <- io . canonicalizePath $ expandHome "/home/zubin" $ projectDirectory proj
      let reldir = makeRelative "/home/zubin" dir
          parents = joinPath $ map (take 5) $ splitPath $ takeDirectory reldir
          prettyDir = Just
                    $ xmobarColor "#fa4a9a" ""
                    $ addTrailingPathSeparator
                    $ normalise
                    $ "~" </> replaceDirectory reldir parents
      pure prettyDir

    nerdBordersPoint fillColor xs = xmobarColor "#2b2b2b" fillColor $ a ++ xmobarBorder "Bottom" fillColor 2 xs ++ b
      where [x,y] = "\57532\57534"
            a = xmobarFont 2 [x]
            b = xmobarFont 2 [y]

    nerdBordersRound xs = (a ++ (xmobarColor "#2b2b2b" color xs) ++ b)
      where [x,y] = "\57534\57532"
            a = "" -- xmobarColor color "#2b2b2b" $ xmobarFont 3 [x]
            b = "" -- xmobarColor color "#2b2b2b" $ xmobarFont 3 [y]
            color = "#cacaca"

myUrgencyHook :: Window -> X ()
myUrgencyHook w = do
  name <- getName w
  safeSpawn "notify-send" ["Urgent!",show name]

toggleScreens :: X ()
toggleScreens = do
  spawn "xrandr --output DP-1 --left-of eDP-1 --mode 2560x1440"
  spawn "kmonad ~/builds/kmonad/gk61.kbd&"

setProjectDir = do
  d <- expandHome "/home/zubin" . projectDirectory <$> currentProject
  io $ setCurrentDirectory d

-- | Send a message to the given workspace
sendToWorkspace :: Message a => WorkspaceId -> a -> X ()
sendToWorkspace t a = windowBracket_ $ do
    scs <- W.screens <$> gets windowset
    mls <- mapM (\w -> if W.tag (W.workspace w) == t then handleMessage (W.layout $ W.workspace w) (SomeMessage a) `catchX` return Nothing else return Nothing) scs
    let (cur:vis) = [ sc{ W.workspace = (W.workspace sc) { W.layout = fromMaybe oldL ml }}
                  | (sc,ml) <- zip scs mls
                  , let oldL = W.layout $ W.workspace sc
                  ]
    modifyWindowSet $ \ws -> ws { W.current = cur
                                , W.visible = vis
                                }
    return (Any True)


myServer :: [String] -> X ()
myServer ["workspace","next"] = moveTo Next (Not emptyWS)
myServer ["workspace","prev"] = moveTo Prev (Not emptyWS)
myServer ["scratchpad",x] = namedScratchpadAction scratchpads x
myServer ["project",name] = lookupProject name >>= \case
  Nothing | null name -> safeSpawn "notify-send" ["empty project"]
          | otherwise -> switchProject (defProject name)
  Just p -> switchProject p
myServer ["mono",x] = withWindowSet $ \ws -> case W.peek $ W.view x ws of
  Nothing -> safeSpawn "notify-send" ["no window", x]
  Just w -> sendToWorkspace x (ToggleFullscreen w)
myServer ["full",x] = withWindowSet $ \ws -> case W.peek $ W.view x ws of
  Nothing -> safeSpawn "notify-send" ["no window", x]
  Just w -> floatFull' (sendToWorkspace x) w
myServer ["hook",x] = toggleHookNext x
myServer ["migrate",readMaybe -> Just w, readMaybe -> Just x] = sendMessage $ Migrate w (x :: Window)
myServer ["cd",dir] = do
  dir' <- liftIO $ makeAbsolute dir
  modifyProject (\p -> p { projectDirectory = dir' })
myServer ["focus",readMaybe -> Just w] = focus w
myServer ["cyclews"] = cycleWorkspacesOnScreens
myServer xs = safeSpawn "notify-send" ["Couldn't recognize cmd", unwords xs]

data ExtT = PersistentE | StateE
newtype Ext (t :: ExtT) a = Ext a
instance (ExtensionClass a, Default a, Read a, Show a) => ExtensionClass (Ext PersistentE a) where
  initialValue = Ext def
  extensionType = PersistentExtension @a . coerce

instance (Default a, ExtensionClass a) => ExtensionClass (Ext StateE a) where
  initialValue = Ext def
  extensionType = StateExtension @a . coerce

isCurMirrored = withWindowSet (fmap (fromMaybe False) . isToggleActive MIRROR . W.workspace . W.current)

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

newtype FocusHistory = FH { getFocusHistory :: [Window] }
  deriving stock (Read, Show, Typeable)
  deriving newtype Default
  deriving ExtensionClass via Ext PersistentE FocusHistory

data FocusLayout a = FL deriving (Read,Show,Typeable)

historyLayout :: l Window -> ModifiedLayout FocusLayout l Window
historyLayout = ModifiedLayout FL

instance LayoutModifier FocusLayout Window where
  modifyLayout fh ws rct = do
    wold <- getFocused
    hist <- getFocusHistory <$> ES.get
    wnew <- windowHistoryHook wold
    case (wnew, wold) of
      (Just w, Just oldw) | w /= oldw -> do
        let oldstack = W.stack ws
        let mw = L.find (`elem` W.integrate' oldstack) hist
        let newstack = if (w `elem` (W.integrate' oldstack))
                       then until ((w==) . W.focus) W.focusUp' <$> oldstack
                       else case mw of
                         Just w -> until ((w==) . W.focus) W.focusUp' <$> oldstack
                         Nothing -> oldstack
        modifyWindowSet (W.focusWindow w)
        addAction $ do
          makeBorderNormal oldw
          windows id
        runLayout ws{W.stack = newstack} rct
      _ -> runLayout ws rct

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
              ES.put $! (FH $! force $ hist')
              return $ L.find (\x -> x `elem` curws ) hist'
          -- Add current focus to history
          | otherwise -> do
              ES.put $! FH $! force $ (w:L.delete w hist)
              return Nothing

addFH Nothing = pure ()
addFH (Just w) = do
  hist <- getFocusHistory <$> ES.get
  ES.put (FH $ w : L.delete w hist)

raiseFloating w = do
  isFloat <- gets $ M.member w . W.floating . windowset
  if isFloat
  then windows $ (W.shiftMaster .) $ W.focusWindow w
  else return ()

updateMode = do
  conf <- asks config
  logHook conf

updateMode' = do
  mode <- ES.gets M.label
  willMerge <- willHookNext "merge"
  m <- isCurMirrored
  let layouticon = "%{T3}"++(if m then "\57354" else "\57353")++"%{T-}"
  let modeColor = case mode of
        "nav" -> "#fa0"
        "minimal" -> "#f40"
        _ -> "#cacaca"
  let prettyMode
        | m || mode /= "normal" = Just $ "%{F"++modeColor ++ "}%{R}\57532" ++ layouticon ++ " " ++ mode ++ "\57534%{R}%{F-}"
        | otherwise = Nothing
  dir <- io . canonicalizePath =<< expandHome "/home/zubin" . projectDirectory <$> currentProject
  let reldir = makeRelative "/home/zubin" dir
      parents = joinPath $ map (take 2) $ splitPath $ takeDirectory reldir
      prettyDir = Just
                $ addTrailingPathSeparator
                $ normalise
                $ "~" </> replaceDirectory reldir parents

  lbranch <- L.trim <$> runProcessWithInput "git" ["-C",dir, "rev-parse","--abbrev-ref","HEAD"] ""
  let branch
        | length lbranch >= 12 = take 5 lbranch ++ "…" ++ L.takeEnd 6 lbranch
        | otherwise = lbranch
      branchText
        | branch /= "master" = Just $ "%{F#faa}"++branch++"%{F}"
        | otherwise = Nothing
      mergeText
        | willMerge = Just "merge"
        | otherwise = Nothing
  io $ appendFile "/tmp/.xmonad-mode-log" $ (++ "\n") $ unwords $ catMaybes [prettyDir, prettyMode, mergeText, branchText]

expandHome :: FilePath -> FilePath -> FilePath
expandHome home dir = case L.stripPrefix "~" dir of
  Nothing -> dir
  Just xs -> home ++ xs

colorMarked = getMarked >>= mapM_ makeBorderRed

myWorkspaces =
            ["main"
            ,"irc"
            ,"web"
            ,"dev"
            ,"conf"
            ,"play"
            ,"media"
            ]

instance Shrinker CustomShrink where
  shrinkIt s cs
    | "Kakoune" `L.isSuffixOf` cs = shrinkKak cs
    | otherwise = shrinkIt shrinkText cs

shrinkKak cs = cs : cs' : cs'' : moarShort shortDirs
  where
    words = L.splitOn " - " cs
    cs'  = L.intercalate " - " $ L.dropEnd 1 words
    cs'' = L.intercalate " - " $ L.dropEnd 2 words
    fs = L.splitOn "/" cs''
    shortDir = case fs of
      [] -> []
      xs -> map (take 2) (init fs) ++ [last fs]
    shortDirs = map (L.intercalate "/") $ init $ L.tails shortDir
    moarShort [] = []
    moarShort [x] = shrinkIt shrinkText x
    moarShort (x:xs) = x : moarShort xs

myLayout = fullscreenFloat
         $ toggleLayouts (noBorders StateFull)
         $ smartBorders
         $ avoidStruts
         $ fullscreenFocus
         -- $ maximizeWithPadding 0
         $ layoutHintsWithPlacement (0.5,0.5)
         $ layouts
          where
              layouts = addTabs CustomShrink myTabTheme
                $ trackFloating $ subLayout [] (Simplest ||| (Mirror $ Column 1)) $ mkToggle1 MIRROR myTall
              myTall = spacing 0 $ resizable 40 5
              resizable step n = ResizableTall 1 (1/step) ((1/2)+n/step) [] (I [])
              -- ^ n is no of step-lengths right of center, which is used as the
              -- default split ratio

kittyPopup = "kitty -1 -o background_opacity=0.95"
scratchpads =
  [ NS "pavucontrol" "pavucontrol" (resource =? "pavucontrol") defaultFloating
  , NS "ncmpcpp" (kittyPopup++" --class ncmpcpp -e ncmpcpp") (resource =? "ncmpcpp") defaultFloating
  , NS "htop" (kittyPopup++" --class htop -e htop") (resource =? "htop") defaultFloating
  , NS "ytop" (kittyPopup++" --class ytop -e ytop -p") (resource =? "ytop") defaultFloating
  , NS "battop" (kittyPopup++" --class battop -e battop") (resource =? "battop") defaultFloating
  , NS "bandwhich" (kittyPopup++" --class bandwhich -e bandwhich") (resource =? "bandwhich") defaultFloating
  , NS "clerk" (kittyPopup++" --class clerk -e clerk") (className =? "clerk") defaultFloating
  , NS "calcurse" (kittyPopup++" --class calcurse -e calcurse -q") (className =? "calcurse") defaultFloating
  , NS "neomutt" (kittyPopup++" --class neomutt -e neomutt") (className =? "neomutt") defaultFloating
  , NS "mpvytdl" "notify-send 'No video scratchpad!'" (resource =? "mpvytdl") defaultFloating
  , NS "dynamic" ("notify-send 'No dynamic scratchpad!'") dynamicScratchpadQuery defaultFloating
  , NS "scratchpad" (unwords ["cd ~;",kittyPopup,"--class scratchpad -e ~/scripts/detachable"]) (resource =? "scratchpad") (customFloating scratchpadDefaultRect)
  ]

scratchpadDefaultRect = W.RationalRect 0.25 0.375 0.5 0.25

newtype DynamicScratchpad
  = DS { getDynamicScratchpad :: Maybe Window }
  deriving stock (Eq,Read,Show)
  deriving newtype Default
  deriving ExtensionClass via Ext PersistentE DynamicScratchpad

dynamicScratchpadQuery = do
  w <- ask
  liftX $ ES.gets ((Just w ==) . getDynamicScratchpad)

makeDynamicScratchpad :: X ()
makeDynamicScratchpad = do
  new <- getFocused
  DS old <- ES.get
  ES.put . DS =<< updateState old new
  where
    removeExisting = do
      spawn "notify-send 'Removing dynamic scratchpad!'"
      pure Nothing

    -- Add if new window
    updateState _ Nothing = removeExisting
    updateState old (Just w) = do
      case old of
        Just w' | w == w' -> removeExisting
        _ -> do
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

{-
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
-}

mergeIntoFocused = mergeIntoFocusedIf (pure True)

mergeIntoFocusedIf q = do
  w <- ask
  f <- liftX getFocused
  case f of
    Nothing -> return ()
    Just x -> liftX $ addAction $
      whenM (runQuery q x) $
        sendMessage $ Migrate w x
  return mempty

newtype PendingActions = PendingActions { getPending :: [X ()] }
  deriving newtype Default
  deriving ExtensionClass via Ext StateE PendingActions

addAction :: X () -> X ()
addAction x = ES.modify (\(PendingActions xs) -> PendingActions (x:xs))

runAllPending :: X ()
runAllPending = do
  PendingActions xs <- ES.get
  ES.put (PendingActions [])
  sequence_ $ reverse xs

manageApps = composeAll
    [ isFullscreen                     --> doFullFloat
    , stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture"  --> placeHook (smart (0,1)) <> doFloat
    , resource =? "dmenu"              --> doFloat
    , resource =? "mpvytdl"            --> doRectFloat (W.RationalRect 0.5 (15/1080) 0.5 0.5)
    , resource =? "pavucontrol"        --> placeHook ( fixed (1,25/1080) ) <> doFloat
    , resource =? "wicd-client.py"     --> placeHook ( fixed (1,35/1080) ) <> doFloat
    , resource =? "gsimplecal"         --> placeHook ( fixed (1,35/1080) )
    , resource =? "alsamixer"          --> placeHook ( fixed (1,35/1080) ) <> doFloat
    , resource =? "nethogs"            --> placeHook ( fixed (1,35/1080) ) <> doFloat
    , resource =? "progress"           --> placeHook ( fixed (1,35/1080) ) <> doFloat
    , resource =? "runner"             --> placeHook ( fixed (0,1) ) <> doFloat
    , resource =? "dzen2"              --> doIgnore
    , resource =? "Dunst"              --> doIgnore
    , resource =? "scratchpad"         --> doRectFloat (centerAligned 0.5 0.3 0.45 0.45)
    , resource =? "htop"               --> doRectFloat (centerAligned 0.75 (14/1080) 0.5 0.65)
    , resource =? "ytop"               --> doRectFloat (centerAligned 0.75 (14/1080) 0.5 0.8)
    , resource =? "battop"             --> doRectFloat (centerAligned 0.75 (14/1080) 0.5 0.6)
    , resource =? "calcurse"           --> doRectFloat (centerAligned 0.80 (14/1080) 0.40 0.5)
    , resource =? "bandwhich"          --> doRectFloat (W.RationalRect 0.42 (14/1080) 0.58 0.6)
    , resource =? "neomutt"            --> doRectFloat (W.RationalRect 0.3 (0.4-22/1080) 0.7 0.6)
    , resource =? "unicodeinp"         --> doRectFloat (centerAligned 0.5 0.3 0.45 0.45)
    , resource =? "kittypopup"         --> doRectFloat (centerAligned 0.5 0.2 0.55 0.65)
    , resource =? "xmonadrestart"      --> doRectFloat (centerAligned 0.5 0.3 0.35 0.35)
    , resource =? "xmessage"           --> doRectFloat (centerAligned 0.5 0.3 0.35 0.35)
    , resource =? "ncmpcpp"            --> doRectFloat (centerAligned 0.5 (14/1080) (2/3) 0.6)
    , resource =? "clerk"              --> placeHook ( fixed (0.5,55/1080) ) <> doFloat
    , resource =? "org.pwmt.zathura"   --> mergeIntoFocusedIf (not <$> className =? "firefox")
                                        <* liftX (addAction $ saveWindows False)
    , resource =? "vimiv"              --> mergeIntoFocusedIf (not <$> className =? "firefox")
                                        <* liftX (addAction $ saveWindows False)
    , className =? "mpv"               --> mergeIntoFocusedIf (not <$> className =? "firefox")
                                        <* liftX (addAction $ saveWindows False)
    , resource =? "gnome-pie"          --> doIgnore <> hasBorder False
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
             , placeHook (smart (1/2,1/2))
             , toggleHook "merge" mergeIntoFocused
             , namedScratchpadManageHook scratchpads
             ]

raiseNew :: ManageHook
raiseNew = do
  w <- ask
  doF $ \ws ->
    if (M.member w $ W.floating ws)
    then W.shiftMaster $ W.focusWindow w ws
    else ws

restartXMonad = do
  writeStateToFile
  spawn (kittyPopup++" --class xmonadrestart -e /home/zubin/.xmonad/restart-cabal.sh")
  broadcastMessage ReleaseResources
  io . flush =<< asks display

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
      [("M-C-<Esc>", setMode (navMode c))
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

mergeNext = hookNext "merge" True

getTerm = do
  -- Project name _ _ <- currentProject
  let name = "regular"
  pure $ myTerm ++ " --single-instance --instance-group="++name

spawnTerm = getTerm >>= spawnHere

appLaunchBindings =
    [("M-S-t", mergeNext >> spawnTerm)
    ,("M-<Return>", spawnTerm)
    ,("M-S-b", spawnHere "firefox")
    -- ,("M-S-e", spawnHere "~/scripts/kak-run -e rofi-files")
    -- ,("M-C-b", spawnHere "~/scripts/kak-run -e rofi-buffers")
    -- ,("M-S-e", mergeNext >> spawnHere "kitty -1 -e zsh -c 'source ~/.zsh_funcs && e'")
    ,("M-S-e", mergeNext >> spawn "kitty -1 -e kak-project -e 'try rofi-files catch quit'")
    ,("M-C-e", spawn "kitty -1 -e kak-project -e 'try rofi-files catch quit'")
    -- ,("M-C-b", spawn "ROFI_SEARCH='ddgr' rofi -modi blocks -blocks-wrap rofi-search -show blocks -lines 10 -eh 4 -kb-custom-1 'Control+y' -color-window 'argb:f32b2b2b, argb:B3000000, argb:E6ffffff'")
    ,("M-u", spawn "kitty --class unicodeinp -o background_opacity=0.90 -e sh -c '(kitty +kitten unicode_input | tr -d \"\\n\"| xsel)' && xdotool click 2")
    ,("M-S-f", spawnHere =<< runInTerm Nothing "ranger")
    ,("M-g", namedScratchpadAction scratchpads "scratchpad" )
    ,("<Insert>", pasteSelection)

    ,("M-i", dynamicScratchpadAction)
    ,("M-S-i", makeDynamicScratchpad)
    ,("M-q", restartXMonad)
    ,("C-q", pure ()) -- Disable it in firefox
    ,("M-r", shellPrompt myXPConfig)
    ,("M-S-z", zshPrompt myXPConfig "/home/zubin/scripts/capture.zsh")
    ,("M-S-v", spawn "~/scripts/playvid.sh")
    ,("M-S-=", spawn "~/scripts/html.sh")
    -- ,("M-S-g", spawn =<< runInTerm "aria2c" "~/scripts/download.sh $(xsel --output --clipboard)")
    ,("M-C-p", spawn "rofi-pass")
    ,("M-S-c", spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'")
    ,("M-S-s", saveWindows True)
    ,("M-S-r", restoreWindows)
    ,("M-M1-<Backspace>", spawn "~/scripts/lock")
    ,("<Print>", spawn "maim -u ~/$(date '+%Y-%m-%d-%H%m%S_grab.png')")
    ,("S-<Print>", spawn "maim -us ~/$(date '+%Y-%m-%d-%H%m%S_grab.png')")
    ,("M-<Print>", spawn "maim -ui $(xdotool getactivewindow) ~/$(date '+%Y-%m-%d-%H%m%S_grab.png')")
    ,("M-C-g", openGHC)
    -- Dunst
    ,("C-`", spawn "dunstctl history-pop")
    ,("C-<Space>", spawn "dunstctl close")
    ,("C-S-<Space>", spawn "dunstctl action")
    ,("C-M1-t", scratchPieMenu)
    ] ++ namedScratchpadActions
    where
      namedScratchpadActions = [("M-v", mkVisualBindings (map (fmap (namedScratchpadAction scratchpads)) namedScratchpadPads))]

namedScratchpadPads =
  [ KeyBindL "h" "htop"
  , KeyBindL "m" "ncmpcpp"
  , KeyBindL "n" "neomutt"
  , KeyBindL "c" "calcurse"
  , KeyBindL "v" "pavucontrol"
  , KeyBindL "f" "mpvytdl"
  ]

scratchPieMenu = runProcessWithInput "pmenu" [] (unlines $ map name scratchpads) >>= namedScratchpadAction scratchpads . L.trim

openGHC :: X ()
openGHC = getSelection >>= \case
  '#':xs -> spawn $ "firefox 'https://gitlab.haskell.org/ghc/ghc/-/issues/"++xs++"'"
  '!':xs -> spawn $ "firefox 'https://gitlab.haskell.org/ghc/ghc/-/merge_requests/"++xs++"'"
  _ -> pure ()

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
    Just w -> floatFull' sendMessage w

floatFull' :: (forall a. Message a => a -> X ()) -> Window -> X ()
floatFull' sendMessage w = do
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

cycleOptions w = filter (/= "NSP") $ map W.tag $ tail (W.workspaces w) ++ [head (W.workspaces w)]

xmonadControlBindings = addSuperPrefix xmonadControlKeys

xmonadControlKeys =
    [("n", windows W.focusDown)
    ,("p", windows W.focusUp)
    ,("f", withFocused (broadcastMessage . ToggleFullscreen) >> sendMessage FullscreenChanged)
    ,("y", floatFull)
    ,("S-n", windows W.swapDown)
    ,("S-p", windows W.swapUp)
    ,("S-u", focusUrgent >> clearUrgents)
    ,("M1-d", cycleWorkspacesOnScreens)
    ,("S-d", cycleFocusOnScreens)
    ,("M1-e", sendCurrentWindowToNext)
    ,("`", toggleWS' ["NSP"])
    ,("S-x", kill1)
    ,("<Tab>",cycleWindowSets cycleOptions [xK_Super_L] xK_Tab xK_grave)
    ,("C-<Return>", dwmpromote )
    ,("S-m", toggleHookNext "merge" >> updateMode )
    ,("C-r", refresh)
    ,("x", killCopy)
    ,("C-d", sendMessage $ SPACING $ negate 5)
    ,("C-i", sendMessage $ SPACING 5)
    ,("b", sendMessage ToggleStruts)
    ,("z", sendMessage (MT.Toggle MIRROR) >> updateMode )
    ,("C-S-d", removeWorkspace >> saveProjectState)
    ,(";", switchProjectPrompt myXPConfig >> saveProjectState)
    ,("d", changeProjectDirPrompt myXPConfig >> saveProjectState >> updateMode)
    ,("w", shiftToProjectPrompt myXPConfig)
    ,("e", dwmpromote )
    ,("/", windowPrompt highlightConfig Goto allWindows)
    ,("C-/", tabPrompt)
    ,("S-/", windowMultiPrompt highlightConfig [(bringAsTabbed,allWindows),(Bring,allWindows)])
    ,("\\" , windowMultiPrompt highlightConfig [(bringCopyAsTabbed, allWindows),(BringCopy,allWindows)])
    ,("<Space>", switchLayer)
    ,("S-<Space>", sendMessage NextLayout)
    ,("C-S-<Space>", toSubl NextLayout)
    ,("'", markFocused)
    ,("a", mergeMarked)
    ,("s", swapWithMarked)
    ,("o", shiftMarked)
    ,("c", spawn "LANG=en_IN.UTF-8 rofi -show calc -theme Arc-Dark -location 2 -no-show-match -no-sort -calc-command \"echo -n '{result}' | xsel -b\" ")
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

sendCurrentWindowToNext = 
  windows $ \ws -> do
    case W.visible ws of
      [] -> ws
      ((W.tag . W.workspace -> w):_) ->  W.view w $ W.shift w ws


cycleWorkspacesOnScreens =
  windows $ \ws ->
    let screens = W.current ws :| W.visible ws
        visibleWs = fmap W.workspace screens
        visibleWs' = rotateNE 1 visibleWs
        current' :| visible' = NE.zipWith (\sc w' -> sc { W.workspace  = w'}) screens visibleWs'

        rotateNE n xs = NE.fromList $ NE.drop n $ NE.cycle xs
      in ws { W.current = current', W.visible = visible' }

cycleFocusOnScreens =
  windows $ \ws ->
    let screens = W.current ws :| W.visible ws
        current' :| visible' = rotateNE 1 screens

        rotateNE n xs = NE.zipWith (flip const) screens $ NE.fromList $ NE.drop n $ NE.cycle xs
      in ws { W.current = current', W.visible = visible' }

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
     ,("M1-"++[key], screenGo dir False)
     ,("S-M1-"++[key], screenSwap dir False)
     ]

mkVisualBindings = visualSubmap myWindow . fmap (\x -> (concat $ showName x, getAction x)) . M.fromList . namedKeys (myConfig [])

myWindow = WindowConfig
 { winFont = "xft:Source Code Pro-10"
 , winBg = "#2b2b2b"
 , winFg = "white"
 , winRect = CenterWindow
 }

mediaBindings =
    [ ("<XF86AudioNext>"        , spawn "playerctl next")
    , ("<XF86AudioPrev>"        , spawn "playerctl previous")
    , ("<XF86AudioPlay>"        , spawn "playerctl play-pause")
    , ("<XF86AudioPause>"       , spawn "playerctl play-pause")
    , ("S-<XF86AudioPlay>"      , spawn "~/scripts/mpris-toggle")
    , ("<XF86AudioStop>"        , spawn "playerctl stop")
    , ("<XF86AudioMute>"        , spawn "~/scripts/dvol2 -t")
    , ("<XF86AudioLowerVolume>" , spawn "~/scripts/dvol2 -d 2")
    , ("<XF86AudioRaiseVolume>" , spawn "~/scripts/dvol2 -i 2")
    , ("<XF86MonBrightnessUp>"  , spawn "light -A 5")
    , ("<XF86MonBrightnessDown>", spawn "light -U 5")
    ] ++ [("M-m", mkVisualBindings mbs)]
mbs =
  [ KeyBindL "m" $ spawn "clerk -t"
  , KeyBindL "n" $ spawn "mpc next"
  , KeyBindL "p" $ spawn "mpc prev"
  , KeyBindL "s" $ spawn "~/scripts/mpd-notify 8000"
  , KeyBindL "<Space>" $ spawn "mpc toggle"
  ]

myTabTheme = def { activeColor = "#cccccc"
                 , activeBorderColor = "#cccccc"
                 , activeTextColor = "#2b2b2b"
                 , inactiveColor = "#2b2b2b"
                 , inactiveBorderColor = "#3c3c3c"
                 , fontName = "xft:Source Code Pro-10"
                 }
myXPConfig = def { position = CenteredAt 0.4 0.5
                 , bgColor = "#333333"
                 , borderColor = "#303030"
                 , promptBorderWidth = 2
                 , font = "xft:Source Code Pro-10"
                 , height = 22
                 , maxComplRows = Just 24
                 , searchPredicate = fuzzyMatch
                 , sorter = fuzzySort
                 }
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
  safeSpawn "/home/zubin/scripts/notesc" ["Restored!",show cmds]

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
  deriving newtype Default
  deriving ExtensionClass via Ext StateE MarkedWindow

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
      windows $ \ss -> W.shiftWin (W.currentTag ss) c ss
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
