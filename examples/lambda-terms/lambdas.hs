-- This module contains an enumeration of well scoped lambda terms
{-# LANGUAGE DeriveDataTypeable #-}

import Test.Feat.Enumerate
import Control.Enumerable
import Test.Feat.Access
import Test.Feat

-- Shows off a bit
main = do
  putStr "There are this many closed lambda terms with 100 lambdas/applications: "
  let n = count 100
  print n
  putStr   $ "Here is one of them: " 
  putStrLn $ pretty (selectTerm 100 (n `div` 2))
  putStrLn "Here are all the terms of size 3:"
  mapM_ (putStrLn . pretty) (snd $ vs !! 3)


data Term scope  = Lam (Term (FinS scope))
                 | App (Term scope) (Term scope)
                 | Var scope
                 deriving (Show, Typeable)
instance Enumerable a => Enumerable (Term a) where
  enumerate  = share $ aconcat 
                 [ c1 Var   -- Variables are size 0, add pay to make size 1
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
  enumerate = share $ aconcat [c0 Z, c1 S]

-- All closed lambda expressions
closed = global :: Enumerate (Term FinZ)
vs = valuesWith closed

-- Count the number of terms of a given size
count n = fst $ vs !! n

-- Select any term of a given size
selectTerm = selectWith closed








-- The rest is just pretty-printing stuff
pretty :: Fin a => Term a -> String
pretty t = prettyPar 0 0 t where
    -- p: 0-never 1-lambda 2-always
    prettyPar :: Fin a => Int -> Int -> Term a -> String
    prettyPar _ _ (Var s) = "x"++show (toInt s)
    prettyPar n p (Lam t) = par 1 p $ "\\x"++show n++"->"++prettyPar (n+1) 0 t
    prettyPar n p (App t1 t2) = par 2 p $
      prettyPar n 1 t1 ++ " " ++ prettyPar n 2 t2
    par p p' s = if p <= p' then "(" ++ s ++ ")" else s

class Fin a where
  toInt :: a -> Int

instance Fin FinZ where
  toInt a = a `seq` error "toInt FinZ"

instance Fin n => Fin (FinS n) where
  toInt Z     = 0
  toInt (S n) = toInt n + 1
