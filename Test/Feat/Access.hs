-- | Functions for accessing the values of enumerations including 
-- compatibility with the property based testing frameworks QuickCheck and
-- SmallCheck.
module Test.Feat.Access(
  -- ** Accessing functions
  index,
  select,
  values,
  striped,
  bounded,
  
  -- ** A simple property tester
  featCheck,

  ioFeat,
  ioAll,
  ioBounded,
  
  Report,
  inputRep,
  prePostRep,
  
  -- ** Compatibility
  -- *** QuickCheck
  uniform,
  -- *** SmallCheck
  toSeries,
  
  -- ** Non-class versions of the access functions
  indexWith,
  selectWith,
  valuesWith,
  stripedWith,
  boundedWith,
  uniformWith,
  toSeriesWith
  )where

-- testing-feat
import Test.Feat.Enumerate 
import Test.Feat.Class
-- base
import Data.List
import Data.Ratio((%))
-- quickcheck
import Test.QuickCheck
-- smallcheck
-- import Test.SmallCheck.Series -- Not needed


-- | Mainly as a proof of concept we define a function to index into
-- an enumeration. (If this is repeated multiple times it might be
-- very inefficient, depending on whether the dictionary for the
-- Enumerable is shared or not.)
index :: Enumerable a => Integer -> a 
index = indexWith optimal

-- | A more fine grained version of index that takes a size and an 
-- index into the values of that size. @select p i@ is only defined for @i@ 
select :: Enumerable a => Int -> Index -> a
select = selectWith optimal

-- | All values of the enumeration by increasing cost (which is the number
-- of constructors for most types). Also contains the cardinality of each list.
values :: Enumerable a => [(Integer,[a])]
values = valuesWith optimal

-- | A generalisation of @values@ that enumerates every nth value of the 
-- enumeration from a given starting point.
-- As a special case @values = striped 0 1@.
--
-- Useful for running enumerations in parallel since e.g. @striped 0 2@ is 
-- disjoint from @striped 0 1 2@ and the union of the two cover all values.
striped ::  Enumerable a => Index -> Integer -> [(Integer,[a])]
striped = stripedWith optimal 

-- | A version of values with a limited number of values in each inner list.
-- If the list corresponds to a Part which is larger than the bound it evenly
-- distributes the values across the enumeration of the Part.
bounded :: Enumerable a => Integer -> [(Integer,[a])]
bounded = boundedWith optimal


-- | Check a property for all values up to a given size.
-- @ featCheck p prop = 'ioAll' p ('inputRep' prop) @
featCheck :: (Enumerable a, Show a) => Int -> (a -> Bool) -> IO ()
featCheck p prop = ioAll p (inputRep prop)

-- | Functions that test a property and reports the result.
type Report a = a -> IO ()

-- | A rather simple but general property testing driver.
-- The property is an (funcurried) IO function that both tests and reports the 
-- error. The driver goes on forever or until the list is exhausted, 
-- reporting its progress and the number of 
-- tests before each new part.
ioFeat :: [(Integer,[a])] -> Report a -> IO ()
ioFeat vs f = go vs 0 0 where
  go ((c,xs):xss) s tot = do
    putStrLn $ "--- Testing "++show c++" values at size " ++ show s
    mapM f xs
    go xss (s+1) (tot + c)
  go []           s tot = putStrLn $ "--- Done. Tested "++ show tot++" values"

-- | Defined as @ioAll p = 'ioFeat' (take p 'values') @
ioAll :: Enumerable a => Int -> Report a -> IO ()
ioAll p = ioFeat (take p values)

-- | Defined as @ioBounded n p = 'ioFeat' (take p $ 'bounded' n)@
ioBounded :: Enumerable a => Integer -> Int -> Report a -> IO ()
ioBounded n p = ioFeat (take p $ bounded n)

-- | Reports counterexamples to the given predicate by printing them
inputRep :: Show a => (a -> Bool) -> Report a
inputRep pred a = if pred a
  then return ()
  else do
    putStrLn "Counterexample found:"
    print a
    putStrLn ""

-- | Takes a function and a predicate on its input/output pairs. 
-- Reports counterexamples by printing the failing input/output pair.
prePostRep :: (Show a, Show b) => (a -> b) -> (a -> b -> Bool) -> Report a
prePostRep f pred a = let fa = f a in if pred a fa
  then return ()
  else do
    putStrLn "Counterexample found. Input:"
    print a
    putStrLn "Output:"
    print fa
    putStrLn ""


-- | Compatibility with QuickCheck. Distribution is uniform generator over 
-- values bounded by the given size. Typical use: @sized uniform@.
uniform :: Enumerable a => Int -> Gen a
uniform = uniformWith optimal

-- | Compatibility with SmallCheck. 
toSeries :: Enumerable a => Int -> [a] 
toSeries = toSeriesWith optimal


-- | Non class version of 'index'.
indexWith :: Enumerate a -> Integer -> a
indexWith e i0 = go (parts e) i0 where
  go (Finite crd ix : ps)  i  = if i < crd then ix i else go ps (i-crd)
  go []                    _  = error $ "index out of bounds: "++show i0


-- | Non class version of 'select'
selectWith :: Enumerate a -> Int -> Index -> a
selectWith e p i = fIndex (parts e  !! p) i


-- | Non class version of 'values'.
valuesWith :: Enumerate a -> [(Integer,[a])]
valuesWith = map fromFinite . parts

-- | Non class version of 'striped'.
stripedWith :: Enumerate a -> Index -> Integer -> [(Integer,[a])]
stripedWith e o0 step = stripedWith' (parts e) o0 where
  stripedWith' []                   o = []
  stripedWith' (Finite crd ix : ps) o = 
    (max 0 d,thisP) : stripedWith' ps o'
    where
      o'     = if space <= 0 then o-crd else step-m-1
      thisP  = map ix (genericTake d $ iterate (+step) o)
      space  = crd - o
      (d,m)  = divMod space step

-- | Non class version of 'bounded'.
boundedWith :: Enumerate a -> Integer -> [(Integer,[a])]
boundedWith e n = map (samplePart n) $ parts e

-- Specification: pick at most @m@ evenly distributed values from part @p@ of @e@
-- Return the list length together with the list of the selected values.
samplePart :: Index -> Finite a -> (Integer,[a])
samplePart m (Finite crd ix) = 
  let  step  =  crd % m
  in if crd <= m
       then (crd,  map ix [0..crd - 1])
       else (m,    map ix [ round (k * step)
                                    | k <- map toRational [0..m-1]])
-- The first value is at index 0 and the last value is at index ~= crd - step
-- This is "fair" if we consider using samplePart on the next part as well.
-- An alternative would be to make the last index used |crd-1|.


-- | Non class version of 'uniform'.
uniformWith :: Enumerate a -> Int -> Gen a
uniformWith = uni . parts where
  uni :: [Finite a] -> Int -> Gen a 
  uni  []  _     =  error "uniform: empty enumeration"
  uni  ps  maxp  =  let  (incl, rest)  = splitAt maxp ps
                         fin           = mconcat incl   
    in  case fCard fin of
          0  -> uni rest 1
          _  -> do  i <- choose (0,fCard fin-1)
                    return (fIndex fin i)   
      
-- | Non class version of 'toSeries'.
toSeriesWith :: Enumerate a -> Int -> [a]
toSeriesWith e d = concat (take (d+1) $ map snd $ valuesWith e)
