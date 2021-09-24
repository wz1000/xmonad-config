{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ResizableTile
-- Copyright   :  (c) MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- More useful tiled layout that allows you to change a width\/height of window.
--
-----------------------------------------------------------------------------

module XMonad.Layout.ResizableTileSub (
                                       -- * Usage
                                       -- $usage
                                       ResizableTall(..), MirrorResize(..)
                                      ) where

import XMonad hiding (tile, splitVertically, splitHorizontallyBy)
import qualified XMonad.StackSet as W
import XMonad.Util.Invisible
import Control.Monad
import qualified Data.Map as M
import Data.List ((\\), foldl')
import Data.Coerce
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ResizableTile
--
-- Then edit your @layoutHook@ by adding the ResizableTile layout:
--
-- > myLayout =  ResizableTall 1 (3/100) (1/2) [] ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You may also want to add the following key bindings:
--
-- > , ((modm,               xK_a), sendMessage MirrorShrink)
-- > , ((modm,               xK_z), sendMessage MirrorExpand)
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data RTMessage = RTResize !Resize | RTMirrorResize !MirrorResize | RTIncMaster !IncMasterN

data MirrorResize = MirrorShrink | MirrorExpand deriving Typeable
instance Message MirrorResize

data ResizableTall a = ResizableTall
    { _nmaster :: !Int       -- ^ number of master windows
    , _delta  :: !Rational   -- ^ change when resizing by 'Shrink', 'Expand',
                            -- 'MirrorShrink', 'MirrorExpand'
    , _frac   :: !Rational   -- ^ width of master
    , _slaves :: ![Rational] -- ^ fraction to multiply the window
                            -- height that would be given when divided equally.
                            --
                            -- slave windows are assigned their modified
                            -- heights in order, from top to bottom
                            --
                            -- unspecified values are replaced by 1
    , _mess   :: !(Invisible [] RTMessage)
    } deriving (Show, Read)

handleMsg :: Eq a => W.Stack a -> [a] -> RTMessage -> ResizableTall a ->  ResizableTall a
handleMsg ms fs !m !(ResizableTall nmaster delta frac mfrac _) = case handleMesg <$> unfloat fs ms of
          Just !x -> x
          Nothing -> ResizableTall nmaster delta frac mfrac (I [])
        where handleMesg s = case m of
                RTResize x -> resize x
                RTMirrorResize x -> mresize x s
                RTIncMaster x -> incmastern x

              unfloat fs s = if W.focus s `elem` fs
                               then Nothing
                               else Just (s { W.up = (W.up s) \\ fs
                                            , W.down = (W.down s) \\ fs })
              resize Shrink = ResizableTall nmaster delta (max 0 $ frac-delta) mfrac (I [])
              resize Expand = ResizableTall nmaster delta (min 1 $ frac+delta) mfrac (I [])
              mresize MirrorShrink s = mresize' s delta
              mresize MirrorExpand s = mresize' s (0-delta)
              mresize' s d = let n = length $ W.up s
                                 total = n + (length $ W.down s) + 1
                                 pos = if n == (nmaster-1) || n == (total-1) then n-1 else n
                                 mfrac' = modifymfrac (mfrac ++ repeat 1) d pos
                             in ResizableTall nmaster delta frac (take total mfrac') (I [])
              modifymfrac [] _ _ = []
              modifymfrac (f:fx) d n | n == 0    = f+d : fx
                                     | otherwise = f : modifymfrac fx d (n-1)
              incmastern (IncMasterN d) = ResizableTall (max 0 (nmaster+d)) delta frac mfrac (I [])

handleMsgs :: Eq a => ResizableTall a -> W.Stack a -> [a] -> NonEmpty RTMessage -> ResizableTall a
handleMsgs l ms fs msgs = foldl' (flip $ handleMsg ms fs) l msgs

unI :: Invisible m a -> m a
unI = coerce

instance LayoutClass ResizableTall Window where
    doLayout l r ms = do
      ml <- case nonEmpty $ unI $ _mess l of
        Nothing -> pure Nothing
        Just msgs -> do
          fs <- M.keys . W.floating <$> gets windowset
          let !l' = handleMsgs l ms fs msgs
          pure $! Just l'
      let ResizableTall nmaster _ frac mfrac _ = fromMaybe l ml
          ws = (zip <*> (tile frac (mfrac ++ repeat 1) r nmaster . length)) (W.integrate ms)
      return (ws, ml)
    pureMessage !l mess = msum
      [ addMsg RTResize
      , addMsg RTMirrorResize
      , addMsg RTIncMaster
      ]
      where !ms = unI $! _mess l
            addMsg :: Message a => (a -> RTMessage) -> Maybe (ResizableTall Window)
            addMsg f = (\x -> l{_mess = I (f x : ms)}) <$> fromMessage mess
    description _ = "ResizableTall"

tile :: Rational -> [Rational] -> Rectangle -> Int -> Int -> [Rectangle]
tile f mf r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically mf n r
    else splitVertically mf nmaster r1 ++ splitVertically (drop nmaster mf) (n-nmaster) r2 -- two columns
  where (r1,r2) = splitHorizontallyBy f r

splitVertically :: RealFrac r => [r] -> Int -> Rectangle -> [Rectangle]
splitVertically [] _ r = [r]
splitVertically _ n r | n < 2 = [r]
splitVertically (f:fx) n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically fx (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = min sh (floor $ fromIntegral (sh `div` fromIntegral n) * f) --hmm, this is a fold or map.

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f
