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

  -- *** Polymorphic sharing
  module Data.Typeable,
  Tag(Source),
  tag,
  eShare,
  noOptim,
  optimise,
  irregular

  ) where

-- testing-feat
import Control.Monad.TagShare(Sharing, runSharing, share)
import Test.Feat.Internals.Tag(Tag(Source))
-- base
import Control.Applicative
import Control.Monad
import Data.Function
import Data.Monoid
import Data.Typeable
import Language.Haskell.TH
import Data.List(transpose)
import Control.Monad.State -- TODO: remove direct dependency on mtl


type Part = Int
type Index = Integer

-- | A functional enumeration of type @t@ is a partition of
-- @t@ into finite numbered sets called Parts. Each parts contains values
-- of a certain cost (typically the size of the value).
data Enumerate a = Enumerate 
   { revParts   ::  RevList (Finite a)
   , optimiser  ::  Sharing Tag (Enumerate a)
   } deriving Typeable    

parts :: Enumerate a -> [Finite a]
parts = fromRev . revParts

fromParts :: [Finite a] -> Enumerate a
fromParts ps = Enumerate (toRev ps) (return $ fromParts ps)

-- | Only use fmap with bijective functions (e.g. data constructors)
instance Functor Enumerate where 
  fmap f e = Enumerate (fmap (fmap f) $ revParts e) (fmap (noOptim . fmap f) $ optimiser e)

-- | Pure is 'singleton' and '<*>' corresponds to cartesian product (as with lists)
instance Applicative Enumerate where
  pure     = singleton
  f <*> a  = fmap (uncurry ($)) (cartesian f a)

-- | The @'mappend'@ is (disjoint) @'union'@
instance Monoid (Enumerate a) where
  mempty      = Enumerate mempty (return mempty)
  mappend     = union
  mconcat     = econcat
  
-- | Optimal 'mconcat' on enumerations.
econcat :: [Enumerate a] -> Enumerate a
econcat []    = mempty
econcat [a]   = a
econcat [a,b] = union a b
econcat xs    = Enumerate 
  (toRev . map mconcat . transpose $ map parts xs)
  (fmap (noOptim . econcat) $ mapM optimiser xs)


-- Product of two enumerations
cartesian (Enumerate xs1 o1) (Enumerate xs2 o2) =
  Enumerate (xs1 `prod` xs2) (fmap noOptim $ liftM2 cartesian o1 o2)

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
union (Enumerate xs1 o1) (Enumerate xs2 o2) =
  Enumerate (xs1 `mappend` xs2) (fmap noOptim $ liftM2 union o1 o2)


-- | The definition of @pure@ for the applicative instance. 
singleton :: a -> Enumerate a
singleton a = Enumerate (revPure $ finPure a) (return (singleton a))


-- | Increases the cost of all values in an enumeration by one.
pay :: Enumerate a -> Enumerate a
pay e = Enumerate (revCons mempty $ revParts e) (fmap (noOptim . pay) $ optimiser e)


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





-------------------------------------------------------
-- Polymorphic sharing

eShare :: Typeable a => Tag -> Enumerate a -> Enumerate a
eShare t e = e{optimiser = share t (optimiser e)}

-- Automatically generates a unique tag based on the source position.
tag :: Q Exp -- :: Tag
tag = location >>= makeTag where
   makeTag Loc{  loc_package  = p,    
                 loc_module   = m,    
                 loc_start    = (r,c) }
       = [|Source p m r c|]

optimise :: Enumerate a -> Enumerate a
optimise e = let e' = runSharing (optimiser e) in
  e'{optimiser = return e'}   

noOptim :: Enumerate a -> Enumerate a
noOptim e = e{optimiser = return e}

-- | Used to avoid non-termination of 'optimise' in the presence of 
-- irregular data types. @irregular@ should be applied to the enumeration for the 
-- constructor that introduces the irregularity. Excessive use may impact 
-- performance
irregular :: Enumerate a -> Enumerate a
irregular e = e{optimiser = gets $ evalState $ optimiser e}

         
--------------------------------------------------------
-- Operations on finite sets
data Finite a = Finite {fCard :: Index, fIndex :: Index -> a}

finEmpty = Finite 0 (\i -> error "index: Empty")

finUnion :: Finite a -> Finite a -> Finite a
finUnion f1 f2 
  | fCard f1 == 0  = f2
  | fCard f2 == 0  = f1
  | otherwise      = Finite car sel where
  car = fCard f1 + fCard f2
  sel i = if i < fCard f1
    then fIndex f1 i
    else fIndex f2 (i-fCard f1)  

instance Functor Finite where
  fmap f fin = fin{fIndex = f . fIndex fin}

instance Applicative Finite where
  pure = finPure
  a <*> b = fmap (uncurry ($)) (finCart a b)
  

instance Monoid (Finite a) where 
  mempty = finEmpty
  mappend = finUnion
  mconcat xs = Finite
    (sum $ map fCard xs)
    (sumSel $ filter ((>0) . fCard) xs)

sumSel :: [Finite a] -> (Index -> a)
sumSel (f:rest) = \i -> if i < fCard f
  then fIndex f i
  else sumSel rest (i-fCard f)
sumSel _        = error "Index out of bounds"

finCart :: Finite a -> Finite b -> Finite (a,b)
finCart f1 f2 = Finite car sel where
  car = fCard f1 * fCard f2
  sel i = let (q, r) = (i `quotRem` fCard f2) 
    in (fIndex f1 q, fIndex f2 r)

finPure :: a -> Finite a
finPure a = Finite 1 one where
  one 0 = a
  one _ = error "index: index out of bounds"


fromFinite :: Finite a -> (Index,[a])
fromFinite (Finite c ix) = (c,map ix [0..c-1])


instance Show a => Show (Finite a) where
  show = show . fromFinite


