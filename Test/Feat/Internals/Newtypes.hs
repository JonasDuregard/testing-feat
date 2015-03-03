{-#LANGUAGE DeriveDataTypeable #-}
module Test.Feat.Internals.Newtypes (
  Infinite(..),
  Nat(..),
  NonZero(..)
  )where

import Data.Typeable

-- | A class of infinite precision integral types. 'Integer' is the principal 
-- class member.
class (Typeable a, Integral a) => Infinite a

instance Infinite Integer

-- | A type of (infinite precision) natural numbers such that @ nat a >= 0 @.
newtype Nat a = Nat {nat :: a} 
  deriving (Typeable, Show, Eq, Ord)

-- | A type of (infinite precision) non-zero integers such that @ nonZero a /= 0 @.
newtype NonZero a = NonZero {nonZero :: a}
  deriving (Typeable, Show, Eq, Ord)
