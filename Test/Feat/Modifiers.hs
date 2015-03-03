{-# LANGUAGE DeriveDataTypeable #-}

-- | Modifiers for types, i.e. newtype wrappers where the values satisfy some 
-- constraint (non-empty, positive etc.). Suggestions on useful types are 
-- appreciated.
--
-- To apply the modifiers types you can use the record label. For instance:
--
-- @
--  data C a = C [a] [a] deriving 'Typeable'
--  instance 'Enumerable' a => 'Enumerable' (C a) where
--     'enumerate' = 'unary' $ 'funcurry' $ 
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
module Test.Feat.Modifiers(
  -- ** List modifiers
  NonEmpty(..),
  mkNonEmpty,

  -- ** Numeric modifiers
  Infinite(..),
  Nat(..),
  NonZero(..),
  
  -- ** Character and string modifiers
  Unicode(..),
  unicodes,
  Printable(..),
  printables
  
  ) where

-- testing-feat
import Test.Feat.Enumerate 
import Test.Feat.Class
import Test.Feat.Internals.Newtypes
-- quickcheck -- Should be made compatible at some point.
-- import Test.QuickCheck.Modifiers


-- | A type of non empty lists.
newtype NonEmpty a = NonEmpty {nonEmpty :: [a]} 
  deriving (Typeable, Show)
mkNonEmpty :: (a,[a]) -> NonEmpty a
mkNonEmpty (x,xs) = NonEmpty $ x:xs
instance Enumerable a => Enumerable (NonEmpty a) where
  enumerate = unary $ mkNonEmpty


enumerateBounded :: (Enum a) => Int -> Int -> Enumerate a
enumerateBounded from to = let e = Enumerate prts (return e) in e 
  where
    prts = toRev$ map (\p -> Finite (crd p) (sel p)) [0..]
    crd p
       | p <= 0          = 0
       | p == 1          = 1
       | 2^(p-1) > num   = max 0 (num - 2^(p-2))
       | otherwise       = 2^(p-2)
    sel 1 0 = toEnum from
    sel p i = toEnum $ 2^(p-2) + fromInteger i + from
    num    = toInteger $ to - from

-- | Any unicode character.
newtype Unicode = Unicode {unicode :: Char} 
  deriving (Typeable, Show, Eq, Ord)

instance Enumerable Unicode where
  enumerate = fmap Unicode $ enumerateBounded 
    (fromEnum (minBound :: Char)) 
    (fromEnum (maxBound :: Char))

-- | Smart constructor for unicode strings.
unicodes :: [Unicode] -> String
unicodes = map unicode

-- | Printable ASCII characters
newtype Printable = Printable {printable :: Char}
  deriving (Typeable, Show)

instance Enumerable Printable where
  enumerate = fmap Printable $ enumerateBounded 32 126

-- | Smart constructor for printable ASCII strings
printables :: [Printable] -> String
printables = map printable
  
