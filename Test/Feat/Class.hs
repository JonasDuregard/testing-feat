
module Test.Feat.Class
  ( Enumerable(..)
  , nullary
  , unary
  , funcurry
  , shared
  , consts
  ) where

import Control.Enumerable

-- compatability
{-# DEPRECATED nullary "use c0 instead" #-}
-- nullary :: x -> Memoizable f x
nullary x = c0 x

{-# DEPRECATED unary "use c1 instead" #-}
-- unary :: (Enumerable a, MemoSized f) => (a -> x) -> f x
unary x = c1 x

{-# DEPRECATED shared "use access instead" #-}
shared :: (Sized f, Enumerable a, Typeable f) => Shareable f a
shared = access


funcurry = uncurry

{-# DEPRECATED consts "use datatype instead" #-}
--consts :: (Typeable a, MemoSized f) => [f a] -> Closed (f a)
consts xs = datatype xs
