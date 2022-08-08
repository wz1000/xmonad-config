{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module KeyDesc where
  
import Data.Char

import System.IO
import System.Directory
import System.FilePath
import System.IO.Unsafe

import GHC.Stack
import Data.List
import Data.IORef
import XMonad.Util.NamedActions
import XMonad.Util.EZConfig
import XMonad

data KeyBind a where
  -- Keybinding for the current line
  KeyBindL :: HasCallStack => String -> a -> KeyBind a
  KeyBindD :: String -> String -> a -> KeyBind a

deriving instance Functor KeyBind
deriving instance Show a => Show (KeyBind a)

-- | take off the longest common prefix of spaces
stripIndent :: Int -> [String] -> [String]
stripIndent l xs = map (drop l) xs

takeLineRange :: Int -> Int -> [String] -> [String]
takeLineRange i j xs = take (j - i + 1) $ drop i xs

undescriptive :: Char -> Bool
undescriptive = (`elem` [' ','$'])

dropQuoted :: String -> (Int,String)
dropQuoted = go 0
  where
    go !n [] = (n,[])
    go !n ('\\' : '"' : xs) = go (n+2) xs
    go !n ('"' : xs) = go1 (n+1) xs
    go !n (_   : xs) = go  (n+1) xs

    go1 !n [] = (n,[])
    go1 !n ('\\' : '"' : xs) = go1 (n+2) xs
    go1 !n ('"' : xs) = (n+1,xs)
    go1 !n (_   : xs) = go1 (n+1) xs

{-# NOINLINE fileCache #-}
fileCache :: IORef [(FilePath, [String])]
fileCache = unsafePerformIO $ newIORef []

readFileCached :: FilePath -> IO [String]
readFileCached f' = do
  let f = "/home/zubin/.xmonad" </> f'
  cache <- readIORef fileCache
  case lookup f cache of
    Nothing -> do
      xs <- lines <$> readFile f
      writeIORef fileCache ((f,xs):cache)
      pure xs

    Just xs -> pure xs

namedKeys :: XConfig l -> [KeyBind (X ())] -> [((KeyMask, KeySym), NamedAction)]
namedKeys c xs = mkNamedKeymap c [ (key, addName desc act) | (key, desc, act) <- map keyDesc xs]

getKc :: KeyBind a -> String
getKc  (KeyBindL k _) = show $ getCallStack callStack
getKc  (KeyBindD _ d _) = d

keyDesc :: KeyBind a -> (String,String,a)
keyDesc (KeyBindD k d x) = (k,d,x)
keyDesc (KeyBindL k x) = case filter ((/= "KeyDesc") . srcLocModule . snd) $ getCallStack callStack of
  (call,loc):_ -> unsafePerformIO $ do
    xs <- readFileCached $ srcLocFile loc
    let (first:rest) = takeLineRange  (srcLocStartLine loc - 1) (srcLocEndLine loc -1) xs
        (quoteL,first') = dropQuoted $ drop afterCons first
        (length -> junkL, first'') = span undescriptive first'
        ls = first'' : stripIndent (afterCons + quoteL + junkL) rest

        afterCons = srcLocStartCol loc + length call

    pure $ (k,unwords ls,x)

keyAct :: KeyBind a -> (String, a)
keyAct (KeyBindL k a) = (k,a)
keyAct (KeyBindD k _ a) = (k,a)

samplekeys :: [KeyBind Int]
samplekeys =
  [ KeyBindL "M-a" $ 1+1
  , KeyBindD "M-z" "Description" $ 23
  , KeyBindL "M-x" $ 2 + (5*10 `div` 2)
  , KeyBindL "M-b" $ 10
  , KeyBindL "M-c" $ sum [1..1000]
  , KeyBindL "M-d" 13
  , KeyBindL "M-e" (17)
  , KeyBindL "M-f" (17
                      +
                      29)
  , KeyBindL "M-c" $ sum
                      [1..12345]
  ]
