-- | This module contains a (hopefully) manageable subset of the functionality
-- of Feat. The rest resides only in the Test.Feat.* modules.
module Test.Feat(
  Enumerate(),
  -- * The type class
  Enumerable(..),
  shared,
  nullary,
  unary,
  FreePair(..),
  funcurry,
  consts,
  -- ** Automatic derivation
  deriveEnumerable,
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
  inputRep
  ) where

import Test.Feat.Access
import Test.Feat.Class
import Test.Feat.Enumerate
-- import Test.Feat.Modifiers
