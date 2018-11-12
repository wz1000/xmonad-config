
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.Modal
-- Description :  Implements true modality in xmonad key-bindings.
-- Copyright   :  (c) 2018  L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  unportable
--
-- Stores the current mode and modal bindings in @ExtensibleState@, then sets
-- hooks to grab and handle @KeyEvent@s as determined by the mode.
--
--------------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{

--module XMonad.Hooks.Modal (
module Modal (
 -- * Usage
 -- $Usage
 modal, setTo, Mode (..),
 Grab (..),
 -- ** Provided Modes
 -- $ProvidedModes
 defaultMode, normalMode, insertMode
) where

-- core
import XMonad

-- contrib
import qualified XMonad.Util.ExtensibleState as XS
--import qualified ExtensibleState as XS
--import XMonad.Util.Grab
import Grab

-- base
import qualified Data.Map.Strict as M
import Data.Bits ((.&.), complement)
import Data.Semigroup (All (..), (<>))

-- }}}

-- --< Usage >-- {{{

-- $Usage
--
-- To use this module, the user config /must/ be passed through 'modal'; this
-- sets up an initial mode and adds hooks causing xmonad to use the bindings
-- for this mode rather than those in the configuration's @keys@ field, which is
-- then completely ignored.
--
-- The provided @Mode@s can be used as follows:
--
-- >
-- > import XMonad
-- > import XMonad.Hooks.Modal
-- >
-- > main :: IO ()
-- > main = xmonad . modal defaultMode $ def
-- >
--
-- However the provided @Mode@s are not personalised so they're essentially just
-- examples. For convenience of reference, their definitions are duplicated sans
-- comments below; copy and modify them to meet your needs.
--
-- >
-- > defaultMode, normalMode, insertMode :: XConfig Layout -> Mode
-- >
-- > defaultMode cnf
-- >   = Mode "DEFAULT" GrabBound
-- >   $ M.insert (modMask cnf, xK_Escape)
-- >              (setTo $ normalMode cnf) (keys def cnf)
-- >
-- > normalMode cnf
-- >   = Mode "NORMAL" GrabAll
-- >   $ M.fromList
-- >     [ ((noModMask, xK_d), setTo $ defaultMode cnf)
-- >     , ((noModMask, xK_i), setTo $  insertMode cnf)
-- >     , ((noModMask, xK_a), setTo $  insertMode cnf)
-- >     ] `M.union` un (modMask cnf) (keys def cnf)
-- >   where un mask = M.mapKeys $ \(m, k) -> (m .&. complement mask, k)
-- >
-- > insertMode cnf
-- >   = Mode "INSERT" GrabBound
-- >   $ M.singleton (shiftMask, xK_Escape) (setTo $ normalMode cnf)
-- >
--
-- You may then want to import this module hiding these values:
--
-- > import XMonad.Hooks.Modal
-- >   hiding (defaultMode, normalMode, insertMode)
--
-- or import the other types and values explicitly:
--
-- > import XMonad.Hooks.Modal (modal, setTo, Mode (..))
--

-- }}}

-- --< Types >-- {{{

-- | A data type for use as a parameter to a @Mode@; encodes whether xmonad
--   should grab all keys or only those that are bound.
data Grab = GrabAll | GrabBound
  deriving (Read, Show, Eq, Ord)

-- | A data type describing a mode; holds a label, the desired @Grab@ behaviour,
--   and the bound actions.
data Mode = Mode
  { label        :: !String
  , keysToGrab   :: !Grab
  , boundActions :: !(M.Map (KeyMask, KeySym) (X ()))
  } deriving Typeable

instance ExtensionClass Mode where
  initialValue = Mode "EMPTY" GrabBound M.empty

-- }}}

-- --< Private >-- {{{

-- | Grab key presses as dictated by the current @Mode@.
regrab :: X ()
regrab = XS.gets keysToGrab >>= \case
  GrabAll   -> grabKP anyModifier anyKey
  GrabBound -> grab . M.keys =<< XS.gets boundActions

-- | Logic shamelessly copied from XMonad.Main source, then tweaked.
modalEventHook :: Event -> X All
modalEventHook = customRegrabEvHook regrab <> \case
  KeyEvent { ev_event_type = t, ev_state = m, ev_keycode = code }
    | t == keyPress -> withDisplay $ \dpy -> do
      kp  <- (,) <$> cleanMask m <*> io (keycodeToKeysym dpy code 0)
      kbs <- XS.gets boundActions
      userCodeDef () (whenJust (M.lookup kp kbs) id)
      pure (All False)
  _ -> pure (All True)

-- }}}

-- --< Public >-- {{{

-- | Given a function to compute the initial @Mode@ from the /final/ user
--   config, modify the supplied user config to use modal bindings.
modal :: (XConfig Layout -> Mode) -> XConfig l -> XConfig l
modal initialMode uConf = uConf
  { startupHook     = startupHook uConf <> (setTo . initialMode =<< asks config)
  , handleEventHook = handleEventHook uConf <> modalEventHook
  }

-- | Set the current @Mode@. It's important to use this function rather than
--   @XS.put@, otherwise xmonad will not grab the correct keys.
setTo :: Mode -> X ()
setTo md = XS.put md >> regrab

-- $ProvidedModes
--
-- These constitute little more than example @Mode@s; use them as a basis for
-- your personalised keybinds.
--

-- | In this @Mode@ the xmonad defaults are used, except that @mod + escape@
--   exits to @normalMode@.
defaultMode :: XConfig Layout -> Mode
defaultMode cnf
  = Mode "DEFAULT" GrabBound
  $ M.insert (modMask cnf, xK_Escape) (setTo $ normalMode cnf) (keys cnf cnf)

-- | In this @Mode@, a version of the xmonad defaults without the @modMask@
--   modifier is used, except that @d@ exits back to the defaults, @i@ and @a@
--   enter @insertMode@, and all key presses are grabbed.
normalMode :: XConfig Layout -> Mode
normalMode cnf
  = Mode "NORMAL" GrabAll
  $ M.fromList
    [ ((noModMask, xK_d), setTo $ defaultMode cnf)
    , ((noModMask, xK_i), setTo $  insertMode cnf)
    , ((noModMask, xK_a), setTo $  insertMode cnf)
    ] `M.union` un (modMask cnf) (keys cnf cnf)
  where un mask = M.mapKeys $ \(m, k) -> (m .&. complement mask, k)

-- | In this @Mode@, only one key press is grabbed: @shift + escape@, which exits
--   to @normalMode@.
insertMode :: XConfig Layout -> Mode
insertMode cnf
  = Mode "INSERT" GrabBound
  $ M.singleton (shiftMask, xK_Escape) (setTo $ normalMode cnf)

-- }}}
