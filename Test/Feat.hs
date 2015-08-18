-- | This module contains a (hopefully) manageable subset of the functionality
-- of Feat. The rest resides only in the Test.Feat.* modules.
module Test.Feat(
  Enumerate(),
  -- * The type class
  Enumerable(..), datatype, c0, c1, c2, c3, c4, c5, c6, c7,

  -- ** Automatic derivation
--  deriveEnumerable,


  -- * Accessing data
  optimal,
  index,
  select,
  values,
  bounded,
  uniform,

  -- ** Testing drivers
  featCheck,
  ioFeat,
  ioAll,
  ioBounded,
  Report,
  inputRep,

  -- * Backwards compatability
  shared,
  nullary,
  unary,
  funcurry,
  consts,

  ) where

import Test.Feat.Access
-- import Test.Feat.Class
import Test.Feat.Enumerate
import Test.Feat.Class
import Control.Enumerable



-- import Test.Feat.Modifiers
