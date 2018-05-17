-- | Functions for accessing the values of enumerations including
-- compatibility with the property based testing framework QuickCheck
module Test.Feat.Access(
  -- * Accessing functions
  optimal,
  index,
  select,
  values,

  -- * QuickCheck Compatibility
  uniform,
  
  -- * Combinators
  skipping,
  bounded,
  sizeRange,

  -- * Non-class versions of the access functions
  indexWith,
  selectWith,
  valuesWith,
  uniformWith
  )where

import Test.Feat.Enumerate
import Control.Enumerable
--import Data.Modifiers

-- base
import Data.List
import Data.Ratio((%))


-- quickcheck
import Test.QuickCheck(choose,Gen)

-- | Memoised enumeration. Note that all cardinalities are kept in memory until your program terminates. 
optimal :: Enumerable a => Enumerate a
optimal = global

-- | Index into an enumeration. Mainly used for party tricks (give it a really large number), since usually you want to distinguish values by size.
index :: Enumerable a => Integer -> a
index = indexWith optimal

-- | A more fine grained version of index that takes a size and an
-- index into the values of that size. @select p i@ is only defined 
-- for @i@ within bounds (meaning @i < fst (values !! p)@).
select :: Enumerable a => Int -> Index -> a
select = selectWith optimal

{-
-- Not too happy with this phantom argument
countThese :: Enumerable a => a -> Int -> Integer
countThese x k = help x (drop k $ parts optimal) where
   help :: a -> [Finite a] -> Integer
   help _ []    = 0
   help _ (f:_) = fCard f
-}
   
-- | All values of the enumeration by increasing cost (which is the number
-- of constructors for most types). Also contains the length of each list.
values :: Enumerable a => [(Integer,[a])]
values = valuesWith optimal


-- | Compatibility with QuickCheck. Distribution is uniform generator over
-- values bounded by the given size. Typical use: @sized uniform@.
uniform :: Enumerable a => Int -> Gen a
uniform = uniformWith optimal

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

                    
-- | Enumerates every nth value of the enumeration from a given starting index.
-- As a special case @striped 0 1@ gives all values (starts at index 0 and takes steps of 1).
--
-- Useful for running enumerations in parallel since e.g. @striped 0 2@ is
-- disjoint from @striped 1 2@ and the union of the two cover all values.
skipping :: Enumerate a -> Index -> Integer -> Enumerate a
skipping _ o0 step | step <= 0 || o0 < 0 = error "skippingWith: invalid argument"
skipping e o0 step = fromParts $ go o0 (parts e) where
   go o []      = []
   go o _       | o < 0 = error "negative"
   go o (p:ps)  = p' : go o' ps where -- error (show (space,take,o')) : 
     space = fCard p - o
     (take,o')  | space <= 0   = (0,o-fCard p)
                | space < step = (1,step-space)
                | otherwise    = (space `quotRem` step)
     p' = Finite{fCard = take 
          , fIndex = \i -> fIndex p (i*step + o)}

-- | A version of values with a limited number of values in each inner list.
-- If the list corresponds to a Part which is larger than the bound it evenly
-- distributes the values across the enumeration of the Part.
bounded :: Enumerate a -> Integer -> Enumerate a
bounded e n = fromParts $ map (samplePart n) $ parts e where
    -- The first value is at index 0 and the last value is at index ~= crd - step
    -- This is "fair" if we consider using samplePart on the next part as well.
    -- An alternative would be to make the last index used |crd-1|.
    samplePart :: Index -> Finite a -> Finite a
    samplePart m f@(Finite crd ix) =
      let  step  =  crd % m
      in if crd <= m
           then f
           else Finite{fCard = m, fIndex = \i -> fIndex f (floor (toRational i * step))}

-- | Remove all sizes exept those in the given inclusive (low,high) range 
sizeRange :: Enumerate a -> (Int, Int) -> Enumerate a
sizeRange e (lo, hi) = fromParts $ take (1+hi-lo) $ drop lo $ parts e


