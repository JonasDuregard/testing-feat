-- | A datatype of finite sequences
module Test.Feat.Finite (Finite (..), Index, fromFinite, finFin) where

import Control.Applicative
import Data.Monoid

type Index = Integer
data Finite a = Finite {fCard :: Index, fIndex :: Index -> a}

finEmpty = Finite 0 (\i -> error "index: Empty")

finUnion :: Finite a -> Finite a -> Finite a
finUnion f1 f2 
  | fCard f1 == 0  = f2
  | fCard f2 == 0  = f1
  | otherwise      = Finite car sel where
  car = fCard f1 + fCard f2
  sel i = if i < fCard f1
    then fIndex f1 i
    else fIndex f2 (i-fCard f1)  

instance Functor Finite where
  fmap f fin = fin{fIndex = f . fIndex fin}

instance Applicative Finite where
  pure = finPure
  a <*> b = fmap (uncurry ($)) (finCart a b)

instance Alternative Finite where
  empty = finEmpty
  (<|>) = finUnion

instance Monoid (Finite a) where 
  mempty = finEmpty
  mappend = finUnion
  mconcat xs = Finite
    (sum $ map fCard xs)
    (sumSel $ filter ((>0) . fCard) xs)

sumSel :: [Finite a] -> (Index -> a)
sumSel (f:rest) = \i -> if i < fCard f
  then fIndex f i
  else sumSel rest (i-fCard f)
sumSel _        = error "Index out of bounds"

finCart :: Finite a -> Finite b -> Finite (a,b)
finCart f1 f2 = Finite car sel where
  car = fCard f1 * fCard f2
  sel i = let (q, r) = (i `quotRem` fCard f2) 
    in (fIndex f1 q, fIndex f2 r)

finPure :: a -> Finite a
finPure a = Finite 1 one where
  one 0 = a
  one _ = error "Index out of bounds"


fromFinite :: Finite a -> (Index,[a])
fromFinite (Finite c ix) = (c,map ix [0..c-1])


instance Show a => Show (Finite a) where
  show = show . fromFinite

finFin :: Integer -> Finite Integer
finFin k | k <= 0 = finEmpty
finFin k = Finite k (\i -> i)
