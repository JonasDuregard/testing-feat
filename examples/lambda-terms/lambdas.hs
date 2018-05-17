-- This module contains an enumeration of well scoped lambda terms
{-# LANGUAGE DeriveDataTypeable #-}

import Test.Feat.Enumerate
import Control.Enumerable
import Test.Feat.Access
import Test.Feat

data Term scope  = Lam (Term (FinS scope))
                 | App (Term scope) (Term scope)
                 | Var scope
                 deriving (Show, Typeable)
instance Enumerable a => Enumerable (Term a) where
  enumerate  = datatype [ c1 Var   -- Variables are size 0, add pay to make size 1
                        , pay (c1 Lam) -- "Irregular constructor"
                        , pay (c2 App)]

-- Finite numeric types
data FinZ deriving Typeable
instance Show FinZ where
  show _ = undefined
instance Enumerable FinZ  where
  enumerate = datatype []
data FinS n = Z | S n deriving (Typeable, Show)
instance Enumerable n => Enumerable (FinS n) where
  enumerate = datatype [c0 Z, c1 S]

-- All closed lambda expressions
closed = global :: Enumerate (Term FinZ)
vs = valuesWith closed

-- Count the number of terms of a given size
count n = fst $ vs !! n

-- Select any term of a given size
selectTerm = selectWith closed
