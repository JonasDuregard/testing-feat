-- | Anexperimental feature to override the 'Enumerable' instance for any type.

module Test.Feat.Class.Override (
  Override,
  noOverride,
  addOverride,
  override
  ) where

import Test.Feat.Enumerate
import Test.Feat.Class
import Test.Feat.Internals.Tag(Tag(Class))
import Test.Feat.Modifiers
import Control.Monad.TagShare
import Control.Monad.State

type Override = DynMap Tag

noOverride :: Override
noOverride  = dynEmpty

addOverride :: Enumerable a => Enumerate a -> Override -> Override
addOverride = dynInsert Class

-- | This function is best described with an example:
-- 
--  @
--    let e1 = override $ addOverride (unary 'printable') noOverride :: Enumerate T
--  @
-- 
-- @e1@ enumerates values of type @T@ where all characters (accessed using 
-- the @Enumerable@ instance for @Char@) are printable. Sometimes this can save 
-- you from placing lots of 'printable' modifiers in your instances or 
-- newtypes in your data type definitions.
--
-- This works for any type (not just characters). This function should typically 
-- not be used when combining enumerations (doing so might increase memory 
-- usage because the resulting enumeration is optimised).
-- Also this only has effect on enumerations which have not already been 
-- optimised, so using override again on the result of override has no effect.
override :: Enumerable a => Override -> Enumerate a
override = evalState (optimiser shared) 


