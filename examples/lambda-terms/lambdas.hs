-- This module contains an enumeration of well scoped lambda terms
{-# LANGUAGE DeriveDataTypeable #-}

import Test.Feat.Enumerate
import Test.Feat.Class
import Test.Feat.Access
import Test.Feat

data Term scope  = Lam (Term (FinS scope))
                 | App (Term scope) (Term scope)
                 | Var scope 
                 deriving (Show, Typeable)
instance Enumerable a => Enumerable (Term a) where
  enumerate  =   unary Var   -- Variables are size 0, add pay to make size 1
             <>  irregular (pay (unary Lam)) -- "Irregular constructor"
             <>  pay (unary (funcurry App))

-- Finite numeric types
data FinZ deriving Typeable
instance Show FinZ where
  show _ = undefined
instance Enumerable FinZ  where
  enumerate = mempty
data FinS n = Z | S n deriving (Typeable, Show)
instance Enumerable n => Enumerable (FinS n) where
  enumerate = pure Z <> fmap S shared

-- All closed lambda expressions
closed = optimal :: Enumerate (Term FinZ)
vs = valuesWith closed

-- Count the number of terms of a given size
count n = fst $ vs !! n

-- Select any term of a given size
selectTerm = selectWith closed

