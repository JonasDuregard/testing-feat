-- | A simple testing driver for testing properties using FEAT.
-- Contains three drivers with different levels of flexibility of configuration. 
--
-- Ironically, this code is mostly untested at the moment. 
module Test.Feat.Driver(
   -- * Simple test driver
   test
   , Result
   , counterexamples
   -- * Test driver with show/readable options
   , testOptions
   , Options(..)
   , defOptions
   -- * Extremely flexible test driver
   , testFlex
   , FlexibleOptions(..)
   , FlexOptions(..)
   , defFlex
   , toFlex
   , toFlexWith
   ) where

import Control.Enumerable
import Test.Feat.Access
import Test.Feat.Finite
import Test.Feat.Enumerate

import System.Timeout
import Data.IORef

-- | Basic options for executing a test. Unlike 'FlexibleOptions' this type has Show/Read instances.
data Options = Options
   { oTimeoutSec :: Maybe Int
   , oSizeFromTo :: Maybe (Int,Int)        -- ^ (first size, last size)
   , oMaxCounter :: Maybe Int              -- ^ Maximum number of counterexamples
   , oSilent     :: Bool
   , oSkipping   :: Maybe (Index, Integer) -- ^ (start-index, steps to skip)
   , oBounded    :: Maybe Integer          -- ^ Maximum number of tests per size
   } deriving (Show,Read)

-- | Much more flexible options for configuring every part of the test execution.
-- @a@ is the parameter type of the property. 
type FlexibleOptions a = IO (FlexOptions a)

-- | FlexOptions
data FlexOptions a = FlexOptions 
   { fIO      :: IO Bool     -> IO (Result a) -- ^ The whole execution of the test is sent through this function.
   , fReport  :: a           -> IO Bool -- ^ Applied to each found counterexample, return False to stop testing
   , fOutput  :: String      -> IO () -- ^ Print text
   , fProcess :: Enumerate a -> Enumerate a -- ^ Applied to the enumeration before running
   , fEnum    :: Enumerate a -- ^ The base enumeration to use, before applying @fProcess@.
   }

data Result a = Exhausted [a] -- ^ Reached max size
              | Quota [a]     -- ^ Reached max number of counterexamples
              | TimedOut [a]
              deriving Show

counterexamples :: Result a -> [a]
counterexamples (Exhausted xs) = xs
counterexamples (Quota xs) = xs
counterexamples (TimedOut xs) = xs

-- | 60 seconds timeout, maximum size of 100, bound of 100000 tests per size
defOptions :: Options
defOptions = Options
  { oTimeoutSec = Just 60
  , oSizeFromTo = Just (0,100)
  , oSilent     = False
  , oSkipping   = Nothing
  , oBounded    = Just 100000
  , oMaxCounter = Just 1
  }

defFlex :: Enumerable a => FlexibleOptions a
defFlex = defFlexWith optimal

-- | For testing without using the 'Enumerable' class.
defFlexWith :: Enumerate a -> FlexibleOptions a
defFlexWith e = toFlexWith e defOptions

toFlex :: Enumerable a => Options -> FlexibleOptions a
toFlex = toFlexWith optimal

toFlexWith :: Enumerate a -> Options -> FlexibleOptions a
toFlexWith e o = do
  res <- newIORef []
  count <- newIORef 0
  let doReport x = do
        modifyIORef res (x:)
        modifyIORef count (+1)
        maybe (return True) (checkCount) (oMaxCounter o)
      checkCount mx = do
        n <- readIORef count
        return (n < mx)
      doIO io = do
        mb <- maybe (fmap Just io) (\t -> timeout (t*1000000) io) (oTimeoutSec o)
        res <- readIORef res 
        return $ maybe (TimedOut res) (\b -> if b then Exhausted res else Quota res) mb
      skip  = maybe id (\(i,n) e -> skipping e i n) (oSkipping o)
      bound = maybe id (\n e -> bounded e n) (oBounded o)
      sizes = maybe id (\bs e -> sizeRange e bs) (oSizeFromTo o)
  return $ FlexOptions
      { fIO = doIO
      , fOutput = if oSilent o then const (return ()) else putStr
      , fReport = doReport
      , fProcess = bound . skip . sizes
      , fEnum = e
      }

-- | Test with default options ('defOptions').
test :: Enumerable a => (a -> Bool) -> IO (Result a)
test = testFlex defFlex

-- | Test with basic options. 
testOptions :: Enumerable a => Options -> (a -> Bool) -> IO (Result a)
testOptions = testFlex . toFlex

-- | The most flexible test driver, can be configured to behave in almost any way.
testFlex :: FlexibleOptions a -> (a -> Bool) -> IO (Result a)
testFlex ioOp p = do
  op <- ioOp
  let e = fProcess op (fEnum op)
      lazyResult = [(n,filter (not . p) xs) | (n,xs) <- valuesWith e]
      runSize k (n,cs) = do 
        fOutput op $ "*** Searching in " ++ show n ++ " values of size " ++ show k ++ "\n"
        doWhile (map (\x -> fOutput op "Counterexample found!\n" >> fReport op x) cs) 
  fIO op ((doWhile $ zipWith runSize [0..] lazyResult))

doWhile :: [IO Bool] -> IO Bool
doWhile [] = return True
doWhile (iob:iobs) = iob >>= \b -> if b then doWhile iobs else return False

