-- | Modifiers for types, i.e. newtype wrappers where the values satisfy some 
-- constraint (non-empty, positive etc.). Suggestions on useful types are 
-- appreciated.
--
-- To apply the modifiers types you can use the record label. For instance:
--
-- @
--  data C a = C [a] [a] deriving 'Typeable'
--  instance 'Enumerable' a => 'Enumerable' (C a) where
--     'enumerate' = 'c2' $ 
--       \\xs ys -> C ('nonEmpty' xs) ('nonEmpty' ys)
-- @
--
-- Alternatively you can put everything in pattern postition:
--
-- @
--  instance 'Enumerable' a => 'Enumerable' (C a) where
--     'enumerate' = 'unary' $ 'funcurry' $ 
--       \\('Free' ('NonEmpty' xs,'NonEmpty' ys)) -> C xs ys)
-- @
--
-- The first approach has the advantage of being usable with a 
-- point free style: @ \\xs -> C ('nonEmpty' xs) . 'nonEmpty' @.
module Test.Feat.Modifiers (module Data.Modifiers) where

import Data.Modifiers

