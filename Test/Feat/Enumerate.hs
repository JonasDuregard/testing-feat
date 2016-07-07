{-#LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

-- | Basic combinators for building enumerations
-- most users will want to use the type class 
-- based combinators in "Test.Feat.Class" instead. 

module Test.Feat.Enumerate (
 

  Index,
  Enumerate(..),
  parts,
  fromParts,
  
  -- ** Reversed lists
  RevList(..),
  toRev,
  
  -- ** Finite ordered sets
  Finite(..),
  fromFinite,
  
  
  -- ** Combinators for building enumerations
  module Data.Monoid,
  union,
  module Control.Applicative,
  cartesian,
  singleton,
  pay,
  ) where

-- testing-feat
-- import Control.Monad.TagShare(Sharing, runSharing, share)
-- import Test.Feat.Internals.Tag(Tag(Source))
-- base
import Control.Sized
import Control.Applicative
import Data.Monoid
import Data.Typeable
import Data.List(transpose)
import Test.Feat.Finite

type Part = Int

-- | A functional enumeration of type @t@ is a partition of
-- @t@ into finite numbered sets called Parts. Each parts contains values
-- of a certain cost (typically the size of the value).
data Enumerate a = Enumerate 
   { revParts   ::  RevList (Finite a)
   } deriving Typeable    

parts :: Enumerate a -> [Finite a]
parts = fromRev . revParts

fromParts :: [Finite a] -> Enumerate a
fromParts ps = Enumerate (toRev ps)

-- | Only use fmap with bijective functions (e.g. data constructors)
instance Functor Enumerate where 
  fmap f e = Enumerate (fmap (fmap f) $ revParts e)

-- | Pure is 'singleton' and '<*>' corresponds to cartesian product (as with lists)
instance Applicative Enumerate where
  pure     = singleton
  f <*> a  = fmap (uncurry ($)) (cartesian f a)

instance Alternative Enumerate where
  empty = Enumerate mempty
  (<|>) = union

instance Sized Enumerate where
  pay e    = Enumerate (revCons mempty $ revParts e)
  aconcat  = mconcat
  pair     = cartesian
  fin k    = fromParts [finFin k]

-- | The @'mappend'@ is (disjoint) @'union'@
instance Monoid (Enumerate a) where
  mempty      = empty
  mappend     = union
  mconcat     = econcat
  
-- | Optimal 'mconcat' on enumerations.
econcat :: [Enumerate a] -> Enumerate a
econcat []    = mempty
econcat [a]   = a
econcat [a,b] = union a b
econcat xs    = Enumerate 
  (toRev . map mconcat . transpose $ map parts xs)


-- Product of two enumerations
cartesian (Enumerate xs1) (Enumerate xs2) = Enumerate (xs1 `prod` xs2)

prod :: RevList (Finite a) -> RevList (Finite b) -> RevList (Finite (a,b))
prod (RevList [] _)           _                 = mempty
prod (RevList xs0@(_:xst) _)  (RevList _ rys0)  = toRev$ prod' rys0 where

  -- We need to thread carefully here, making sure that guarded recursion is safe
  prod' []        = []
  prod' (ry:rys)  = go ry rys where
    go ry rys = conv xs0 ry : case rys of
      (ry':rys')   -> go ry' rys'
      []           -> prod'' ry xst

  -- rys0 is exhausted, slide a window over xs0 until it is exhausted
  prod'' :: [Finite b] -> [Finite a] -> [Finite (a,b)]
  prod'' ry = go where
    go []         = []
    go xs@(_:xs') = conv xs ry : go xs'

  conv :: [Finite a] -> [Finite b] -> Finite (a,b)
  conv xs ys = Finite 
    (sum $ zipWith (*) (map fCard xs) (map fCard ys )) 
    (prodSel xs ys)

  prodSel :: [Finite a] -> [Finite b] -> (Index -> (a,b))
  prodSel (f1:f1s) (f2:f2s) = \i -> 
    let mul = fCard f1 * fCard f2  
    in  if i < mul 
        then  let (q, r) = (i `quotRem` fCard f2) 
              in (fIndex f1 q, fIndex f2 r)
        else prodSel f1s f2s (i-mul)
  prodSel _ _ = \i -> error "index out of bounds"


union :: Enumerate a -> Enumerate a -> Enumerate a
union (Enumerate xs1) (Enumerate xs2) = Enumerate (xs1 `mappend` xs2)


-- | The definition of @pure@ for the applicative instance. 
singleton :: a -> Enumerate a
singleton a = Enumerate (revPure $ pure a)



------------------------------------------------------------------
-- Reverse lists

-- | A data structure that contains a list and the reversals of all initial 
-- segments of the list. Intuitively 
--
-- @reversals xs !! n = reverse (take (n+1) (fromRev xs))@
--
-- Any operation on a @RevList@ typically discards the reversals and constructs
-- new reversals on demand.
data RevList a = RevList {fromRev :: [a], reversals :: [[a]]} deriving Show

instance Functor RevList where
  fmap f = toRev . fmap f . fromRev

-- Maybe this should be append instead?
-- | Padded zip
instance Monoid a => Monoid (RevList a) where
  mempty         = toRev[]
  mappend xs ys  = toRev$ zipMon (fromRev xs) (fromRev ys) where
    zipMon :: Monoid a => [a] -> [a] -> [a]
    zipMon (x:xs) (y:ys) = x <> y : zipMon xs ys
    zipMon xs ys         = xs ++ ys  

-- | Constructs a "Reverse list" variant of a given list. In a sensible 
-- Haskell implementation evaluating any inital segment of 
-- @'reversals' (toRev xs)@ uses linear memory in the size of the segment.
toRev:: [a] -> RevList a
toRev xs = RevList xs $ go [] xs where
  go _ []       = []
  go rev (x:xs) = let rev' = x:rev in rev' : go rev' xs
  
-- | Adds an  element to the head of a @RevList@. Constant memory iff the 
-- the reversals of the resulting list are not evaluated (which is frequently 
-- the case in @Feat@).
revCons a = toRev. (a:) . fromRev

revPure a = RevList [a] [[a]]






