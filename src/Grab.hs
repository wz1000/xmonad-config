
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Grab
-- Description :  TODO
-- Copyright   :  (c) 2018  L. S. Leary -- TODO this is kinda wrong...
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  unportable
--
-- TODO
--
--------------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{

--module XMonad.Util.Grab (
module Grab (
 -- * Usage
 -- $Usage
 grabKP, ungrabKP,
 grabUngrab, grab,
 customRegrabEvHook
) where

-- core
import XMonad

-- base
import qualified Data.Map.Strict as M
import Data.Bits (setBit)
import Data.Semigroup (All (..))
import Data.Traversable (for)
import Data.Foldable (traverse_)
import Control.Monad (when)

-- }}}

-- --< Usage >-- {{{

-- $Usage
--
-- TODO
--

-- }}}

-- --< Public Utils >-- {{{

-- | A more convenient version of @grabKey@.
grabKP :: KeyMask -> KeyCode -> X ()
grabKP mdfr kc = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io (grabKey dpy kc mdfr rootw True grabModeAsync grabModeAsync)

-- | A more convenient version of @ungrabKey@.
ungrabKP :: KeyMask -> KeyCode -> X ()
ungrabKP mdfr kc = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io (ungrabKey dpy kc mdfr rootw)

-- | TODO
grabUngrab :: [(KeyMask, KeySym)] -> [(KeyMask, KeySym)] -> X ()
grabUngrab gr ugr = do
  f <- mkGrabs
  traverse_ (uncurry ungrabKP) (f ugr)
  traverse_ (uncurry   grabKP) (f  gr)

-- | TODO
grab :: [(KeyMask, KeySym)] -> X ()
grab ks = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io (ungrabKey dpy anyKey anyModifier rootw)
  grabUngrab ks []

-- | TODO
-- | Logic shamelessly copied from XMonad.Main source and tweaked.
customRegrabEvHook :: X () -> Event -> X All
customRegrabEvHook regr = \case
  e@(MappingNotifyEvent {}) -> do
    io (refreshKeyboardMapping e)
    when (ev_request e `elem` [mappingKeyboard, mappingModifier]) $
      setNumlockMask >> regr
    pure (All False)
  _ -> pure (All True)

-- }}}

-- --< Private Utils >-- {{{

-- | Private action shamelessly copied and restyled from XMonad.Main source.
setNumlockMask :: X ()
setNumlockMask = withDisplay $ \dpy -> do
  ms <- io (getModifierMapping dpy)
  xs <- sequence
    [ do ks <- io (keycodeToKeysym dpy kc 0)
         pure $ if ks == xK_Num_Lock then setBit 0 (fromIntegral m)
                                     else 0 :: KeyMask
    | (m, kcs) <- ms
    , kc <- kcs
    , kc /= 0
    ]
  modify $ \s -> s { numberlockMask = foldr (.|.) 0 xs }

-- | TODO
-- | Private function shamelessly copied and refactored from XMonad.Main source.
mkGrabs :: X ([(KeyMask, KeySym)] -> [(KeyMask, KeyCode)])
mkGrabs = withDisplay $ \dpy -> do
  let (minCode, maxCode) = displayKeycodes dpy
      allCodes = [fromIntegral minCode .. fromIntegral maxCode]
  syms <- io . for allCodes $ \code -> keycodeToKeysym dpy code 0
  let keysymMap = M.fromListWith (++) (zip syms $ pure <$> allCodes)
      keysymToKeycodes sym = M.findWithDefault [] sym keysymMap
  extraMods <- extraModifiers
  pure $ \ks -> do
    (mask, sym) <- ks
    keycode     <- keysymToKeycodes sym
    extraMod    <- extraMods
    pure (mask .|. extraMod, keycode)

-- }}}
