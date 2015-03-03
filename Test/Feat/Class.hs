{-#LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

-- | Everything you need to construct an enumeration for an algebraic type.
-- Just define each constructor using pure for nullary constructors and 
-- unary and funcurry for positive arity constructors, then combine the 
-- constructors with consts. Example:
-- 
-- @
--  instance Enumerable a => Enumerable [a] where
--    enumerate = consts [unary (funcurry (:)), pure []]
-- @
--
-- There's also a handy Template Haskell function for automatic derivation.


module Test.Feat.Class (
  Enumerable(..),
  
  -- ** Building instances
  Constructor,
  nullary,
  unary,
  funcurry,
  consts,
  
  -- ** Accessing the enumerator of an instance
  shared,
  optimal,
  
  -- *** Free pairs
  FreePair(..),
  
  
  -- ** Deriving instances with template Haskell
  deriveEnumerable,
  deriveEnumerable',
  ConstructorDeriv,
  dAll,
  dExcluding,
  dExcept
  -- autoCon,
  -- autoCons
  ) where

-- testing-feat
import Test.Feat.Enumerate
import Test.Feat.Internals.Tag(Tag(Class))
import Test.Feat.Internals.Derive
import Test.Feat.Internals.Newtypes
-- base
import Data.Typeable
import Data.Monoid
-- template-haskell
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- base - only for instances
import Data.Word
import Data.Int
import Data.Bits
import Data.Ratio

-- | A class of functionally enumerable types
class Typeable a => Enumerable a where
  -- | This is the interface for defining an instance. When combining 
  -- enumerations use 'shared' instead and when accessing the data of 
  -- enumerations use 'optimal'.
  enumerate  :: Enumerate a


-- | Version of 'enumerate' that ensures that the enumeration is shared 
-- between all accesses. Should always be used when 
-- combining enumerations.
shared :: Enumerable a => Enumerate a
shared  = eShare Class enumerate
  
-- | An optimal version of enumerate. Used by all
-- library functions that access enumerated values (but not 
-- by combining functions). Library functions should ensure that 
-- @optimal@ is not reevaluated.
optimal :: Enumerable a => Enumerate a
optimal = optimise shared   

-- | A free pair constructor. The cost of constructing a free pair
-- is equal to the sum of the costs of its components. 
newtype FreePair a b = Free {free :: (a,b)} 
  deriving (Show, Typeable)

-- | Uncurry a function (typically a constructor) to a function on free pairs.
funcurry :: (a -> b -> c) -> FreePair a b -> c
funcurry f = uncurry f . free

instance (Enumerable a, Enumerable b) => 
         Enumerable (FreePair a b) where
  enumerate = curry Free <$> shared <*> shared

type Constructor = Enumerate
  
-- | For nullary constructors such as @True@ and @[]@.
nullary :: a -> Constructor a
nullary = pure

-- | For any non-nullary constructor. Apply 'funcurry' until the type of
-- the result is unary (i.e. n-1 times where n is the number of fields 
-- of the constructor).
unary :: Enumerable a => (a -> b) -> Constructor b
unary f = f <$> shared

-- | Produces the enumeration of a type given the enumerators for each of its
-- constructors. The result of 'unary' should typically not be used 
-- directly in an instance even if it only has one constructor. So you 
-- should apply consts even in that case. 
consts :: [Constructor a] -> Enumerate a
consts xs = pay $ mconcat xs 


--------------------------------------------------------------------
-- Automatic derivation

-- | Derive an instance of Enumberable with Template Haskell. To derive
-- an instance for @Enumerable A@, just put this as a top level declaration 
-- in your module (with the TemplateHaskell extension enabled):
-- 
-- @
--   deriveEnumerable ''A
-- @

deriveEnumerable :: Name -> Q [Dec]
deriveEnumerable = deriveEnumerable' . dAll


type ConstructorDeriv = (Name, [(Name, ExpQ)])
dAll :: Name -> ConstructorDeriv
dAll n = (n,[])
dExcluding :: Name -> ConstructorDeriv -> ConstructorDeriv
dExcluding n (t,nrs) = (t,(n,[|mempty|]):nrs)
dExcept :: Name -> ExpQ -> ConstructorDeriv -> ConstructorDeriv
dExcept n e (t,nrs) = (t,(n,e):nrs)

-- | Derive an instance of Enumberable with Template Haskell, with 
-- rules for some specific constructors
deriveEnumerable' :: ConstructorDeriv -> Q [Dec]
deriveEnumerable' (n,cse) =
  fmap return $ instanceFor ''Enumerable [enumDef] n 
  where
    enumDef :: [(Name,[Type])] -> Q Dec
    enumDef cons = do
      sanityCheck
      fmap mk_freqs_binding [|consts $ex |] 
      where
        ex = listE $ map cone cons
        cone xs@(n,_) = maybe (cone' xs) id $ lookup n cse
        cone' (n,[]) = [|nullary $(conE n)|]
        cone' (n,_:vs) = 
          [|unary $(foldr appE (conE n) (map (const [|funcurry|] ) vs) )|]
        mk_freqs_binding :: Exp -> Dec
        mk_freqs_binding e = ValD (VarP 'enumerate ) (NormalB e) []
        sanityCheck = case filter (`notElem` map fst cons) (map fst cse) of
          [] -> return ()
          xs -> error $ "Invalid constructors for "++show n++": "++show xs
        


---------------------------------------------------------------------
-- Instances

{-
-- There may have been some problems with this TH script on older GHC versions. 
-- Its result is pasted at the end of this file
(let 
  it = mapM (instanceFor ''Enumerable [enumDef]) 
    [ ''[] 
    , ''Bool
    , ''()
    , ''(,)
    , ''(,,)
    , ''(,,,)
    , ''(,,,,)
    , ''(,,,,,)
    , ''(,,,,,,) -- This is as far as typeable goes...
    , ''Either
    , ''Maybe
    , ''Ordering
    ]
  -- Circumventing the stage restrictions by means of code repetition.
  enumDef :: [(Name,[Type])] -> Q Dec
  enumDef cons = fmap mk_freqs_binding [|consts $ex |] where
    ex = listE $ map cone cons
    cone (n,[]) = [|pure $(conE n)|]
    cone (n,_:vs) = 
      [|unary $(foldr appE (conE n) (map (const [|funcurry|] ) vs) )|]
    mk_freqs_binding :: Exp -> Dec
    mk_freqs_binding e = ValD (VarP 'enumerate) (NormalB e) []
  in it)
-}

simpleEnum car sel = 
  let e = Enumerate 
           (toRev$ map (\p -> Finite (car p) (sel p)) [0..])
           (return e)
  in e


-- This instance is quite important. It needs to be exponential for 
-- the other instances to work.
instance Infinite a => Enumerable (Nat a) where 
  enumerate = simpleEnum crd sel 
    where
      crd p
        | p <= 0     = 0
        | p == 1     = 1
        | otherwise  = 2^(p-2)
      sel :: Num a => Int -> Index -> Nat a
      sel 1 0 = Nat 0
      sel p i = Nat $ 2^(p-2) + fromInteger i


-- This instance is used by the Int* instances and needs to be exponential as 
-- well.
instance Enumerable Integer where 
  enumerate = unary f  where
    f (Free (b,Nat i)) = if b then -i-1 else i

instance (Infinite a, Enumerable a) => Enumerable (NonZero a) where 
  enumerate = unary (\a -> NonZero $ if a >= 0 then a+1 else a)            

-- An exported version would have to use $tag instead of Class
word :: (Bits a, Integral a) => Enumerate a 
word = e where
  e = cutOff (bitSize' e+1) $ unary (fromInteger . nat)
  
int :: (Bits a, Integral a) => Enumerate a 
int = e where
  e = cutOff (bitSize' e+1) $ unary fromInteger

cutOff :: Int -> Enumerate a -> Enumerate a 
cutOff n e = Enumerate prts (fmap (cutOff n) (optimiser e)) where
  prts = toRev$ take n $ parts e

bitSize' :: Bits a => f a -> Int
bitSize' f = hlp undefined f where
  hlp :: Bits a => a -> f a -> Int
  hlp a _ = bitSize a

instance Enumerable Word where
  enumerate = word
instance Enumerable Word8 where
  enumerate = word
instance Enumerable Word16 where
  enumerate = word
instance Enumerable Word32 where
  enumerate = word
instance Enumerable Word64 where
  enumerate = word

instance Enumerable Int where
  enumerate = int
instance Enumerable Int8 where
  enumerate = int
instance Enumerable Int16 where
  enumerate = int
instance Enumerable Int32 where
  enumerate = int
instance Enumerable Int64 where
  enumerate = int

-- | Not injective
instance Enumerable Double where
  enumerate = unary (funcurry encodeFloat)

-- | Not injective
instance Enumerable Float where
  enumerate = unary (funcurry encodeFloat)

-- This should be fixed with a bijective function.
-- | Not injective
instance (Infinite a, Enumerable a) => Enumerable (Ratio a) where
  enumerate = unary $ funcurry $ \a b -> a % nonZero b

-- | Contains only ASCII characters
instance Enumerable Char where
  enumerate = cutOff 8 $ unary (toEnum . fromIntegral :: Word -> Char)




---- The rest of this file is automatically generated with -ddump-splices, then adjusted by hand
instance Enumerable a_12 =>
             Enumerable ([] a_12) where
      enumerate
        = consts
            [pure [],
             unary (funcurry (:))]
instance Enumerable Bool where
      enumerate = consts [pure False, pure True]
instance Enumerable () where
      enumerate = consts [pure ()]
instance (Enumerable a_12, Enumerable b_13) =>
             Enumerable ((,) a_12 b_13) where
      enumerate = consts [unary (funcurry (,))]
instance (Enumerable a_12, Enumerable b_13, Enumerable c_14) =>
             Enumerable ((,,) a_12 b_13 c_14) where
      enumerate
        = consts [unary (funcurry (funcurry (,,)))]
instance (Enumerable a_12,
              Enumerable b_13,
              Enumerable c_14,
              Enumerable d_15) =>
             Enumerable ((,,,) a_12 b_13 c_14 d_15) where
      enumerate
        = consts
            [unary (funcurry (funcurry (funcurry (,,,))))]
instance (Enumerable a_12,
              Enumerable b_13,
              Enumerable c_14,
              Enumerable d_15,
              Enumerable e_16) =>
             Enumerable ((,,,,) a_12 b_13 c_14 d_15 e_16) where
      enumerate
        = consts
            [unary
               (funcurry
                  (funcurry (funcurry (funcurry (,,,,)))))]
instance (Enumerable a_12,
              Enumerable b_13,
              Enumerable c_14,
              Enumerable d_15,
              Enumerable e_16,
              Enumerable f_17) =>
             Enumerable ((,,,,,) a_12 b_13 c_14 d_15 e_16 f_17) where
      enumerate
        = consts
            [unary
               (funcurry
                  (funcurry
                     (funcurry (funcurry (funcurry (,,,,,))))))]
instance (Enumerable a_12,
              Enumerable b_13,
              Enumerable c_14,
              Enumerable d_15,
              Enumerable e_16,
              Enumerable f_17,
              Enumerable g_18) =>
             Enumerable ((,,,,,,) a_12 b_13 c_14 d_15 e_16 f_17 g_18) where
      enumerate
        = consts
            [unary
               (funcurry
                  (funcurry
                     (funcurry
                        (funcurry (funcurry (funcurry (,,,,,,)))))))]
instance (Enumerable a_acKx, Enumerable b_acKy) =>
             Enumerable (Either a_acKx b_acKy) where
      enumerate = consts [unary Left, unary Right]
instance Enumerable a_a1aW => Enumerable (Maybe a_a1aW) where
      enumerate = consts [pure Nothing, unary Just]
instance Enumerable Ordering where
      enumerate = consts [pure LT, pure EQ, pure GT]
  
