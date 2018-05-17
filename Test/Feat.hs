{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- | This module contains a (hopefully) manageable subset of the functionality
-- of Feat. The rest resides only in the Test.Feat.* modules.
module Test.Feat(

  -- * Testing driver
  test,
  testOptions,
  Options(..),
  defOptions,

  
  -- * The type class
  Enumerate(),
  Enumerable(..), datatype, c0, c1, c2, c3, c4, c5, c6, c7,

  -- ** Automatic derivation
  deriveEnumerable,


  -- * Accessing data
  optimal,
  index,
  select,
  values,
  uniform,


  ) where

import Test.Feat.Access
-- import Test.Feat.Class
import Test.Feat.Enumerate
import Test.Feat.Driver
import Control.Enumerable


-- import Test.Feat.Modifiers
