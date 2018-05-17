-- This is tested with haskell-src-exts-1.19.1
{-#LANGUAGE MagicHash, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
-- BangPatterns, ScopedTypeVariables, ViewPatterns, KindSignatures


import Language.Haskell.TH.Syntax
  ( Exp(..), Pat(..), Stmt(..), Type(..), Dec(..),
    Range(..), Lit(..), Kind(..),
    Body(..), Guard(..), Con(..), Match(..),
    Name(..), mkName, NameFlavour(..), NameSpace(..),
    Clause(..), Pragma(..), FamFlavour(..),
    Pred(..), TyVarBndr(..),
    Foreign, Callconv(..), FunDep(..),
    Safety(..), Strict(..), OccName(..), ModName(..))
-- testing-feat
import Test.Feat
import Test.Feat.Modifiers
import Control.Enumerable
-- template-haskell
-- import Language.Haskell.TH.Syntax.Internals(OccName(OccName), ModName(ModName), PkgName)
import Language.Haskell.TH.Ppr(pprint,Ppr)
-- haskell-src-meta
-- import Language.Haskell.Meta(toExp)
-- haskell-src-exts
import qualified Language.Haskell.Exts as E
-- quickcheck
import Test.QuickCheck hiding (NonEmpty, (><))
--base
import Data.Typeable(Typeable)
import Data.Ord
import Data.List
-- smallcheck

-- import Test.SmallCheck.Series hiding (Nat)
-- import Test.SmallCheck


main = testOptions defOptions{oMaxCounter=Just 10} prop_parses

type Ex = (E.Exp E.SrcSpanInfo)


-- Haskell parser
myParse :: String -> E.ParseResult Ex
myParse = E.parseWithMode E.defaultParseMode{E.extensions =
     (map E.EnableExtension [E.ExplicitForAll, E.ConstraintKinds])
{-    [ E.BangPatterns
    , E.ScopedTypeVariables
    , E.ViewPatterns
    , E.KindSignatures
    , E.ExplicitForAll
    , E.TypeFamilies
    ]-}
    }

-- | Newtype to make error reporting look nicer
newtype PPR a = PPR a

instance (Show a, Ppr a) => Show (PPR a) where
  show (PPR a) = show a ++ "\n" ++ pprint a ++ "\n" ++errormsg a where
    errormsg x = case myParse (pprint x) of
      E.ParseFailed _ s -> s
      E.ParseOk _     -> "OK"
instance Enumerable a => Enumerable (PPR a) where
  enumerate = share (c1 PPR) 

-- | Every pretty-printer result should be par
prop_parses (PPR e) = case myParse $ pprint (e :: Exp) of
  E.ParseOk _       -> True
  E.ParseFailed _ s -> False

{-

-- Currently both of these spit out a lot of errors unless we disable a few of the
-- buggier constructors (which we have done below).
test_parsesAll = ioAll 15 report_parses
-- | Test (at most) 10000 values of each size up to size 100.
test_parsesBounded = ioBounded 10000 100 report_parses

test_parsesBounded' = ioFeat (boundedWith enumerate 1000) report_parses

report_parses e = case prop_parsesM e of
    Nothing -> return ()
    Just s  -> do
               putStrLn "Failure:"
               putStrLn (pprint e)
               print e
               putStrLn s
               putStrLn ""

prop_parsesM e = case myParse $ pprint (e :: Exp) :: E.ParseResult E.Exp of
  E.ParseOk _       -> Nothing
  E.ParseFailed _ s -> Just s


test_cycleAll = ioAll 15 report_cycle
test_cycleBounded = ioBounded 10000 100 report_cycle
report_cycle e = case prop_cycle e of
    Nothing       -> return ()
    Just (ee,ex)  -> do
               putStrLn "Failure:"
               putStrLn (pprint ex)
               print ex
               putStrLn (E.prettyPrint  ee)
               putStrLn ""

-- Round-trip property: TH -> String -> HSE -> TH
-- Uses haskell-src-meta for HSE -> TH
prop_cycle :: Exp -> Maybe (E.Exp,Exp)
prop_cycle e = case myParse $ pprint (e :: Exp) :: E.ParseResult E.Exp of
  E.ParseOk hse       -> if e == toExp hse then Nothing else Just $ (hse, toExp hse)
  E.ParseFailed _ s   -> Nothing -- Parse failures do not count as errors!






-}



-- These statements are always expressions
newtype ExpStmt = ExpStmt Exp deriving Typeable

-- Declarations allowed in where clauses
newtype WhereDec = WhereDec{unWhere :: Dec} deriving Typeable

-- Lowecase names
newtype LcaseN = LcaseN {lcased :: Name} deriving Typeable
-- Uppercase names
newtype UpcaseName = UpcaseName {ucased :: Name} deriving Typeable
newtype BindN = BindN Name deriving Typeable


newtype CPair a b = CPair {cPair :: (a,b)} deriving Typeable

instance (Enumerable a,Enumerable b) => Enumerable (CPair a b) where
  enumerate = datatype [c1 CPair]

instance Enumerable Exp where
  enumerate = datatype
      [c1 $ VarE . lcased
      ,c1 $ ConE . ucased
      ,c1 LitE
      ,c2 AppE
      ,c1 $ \(ExpStmt a,o)   -> InfixE (Just a) (either ConE VarE o) Nothing
      ,c1 $ \(ExpStmt a,o)   -> InfixE Nothing  (either ConE VarE o) (Just a)
      ,c1 $ \(a,o,b) -> InfixE (Just a) (either ConE VarE o) (Just b)
    --  ,c3 $ \a o b -> UInfixE a (VarE o) b
    --  ,c3 $ \a o b -> UInfixE a (ConE o) b
    --  ,c1 ParensE
      ,c2 $ LamE . nonEmpty
      ,c1 $ \(x1,x2,xs) -> TupE (x1:x2:xs)
    --  ,c1 UnboxedTupE
      ,c3 CondE
      ,c1 $ \(d,ds,e) -> LetE (map unWhere $ d:ds) e -- DISABLED BUGGY EMPTY LETS
      ,c1 $ \(e,NonEmpty m) -> CaseE e m
      ,c1 $ \(ExpStmt e,ss) -> DoE (ss ++ [NoBindS e])
      ,c1 $ (\((p,e),(CPair (xs,e'))) -> CompE ([BindS p e] ++ xs ++ [NoBindS e']))
    --  ,c1 ArithSeqE -- BUGGY!
      ,c1 ListE
    --  ,c2 SigE -- BUGGY!
      ,c1 $ \(e,x) -> RecConE e $ map unCase (nonEmpty x)
      ,c1 $ \(e,fe) -> RecUpdE e $ map unCase (nonEmpty fe)
      ]

unCase (LcaseN n,e) = (n,e)

instance Enumerable ExpStmt where
  enumerate = datatype
      [ c1 $ ExpStmt . VarE
      , c1 $ ExpStmt . ConE
      -- , c1 $ ExpStmt . LitE
      , c1 $ \(e1,e2) -> ExpStmt (AppE e1 e2)
      -- , c1 $ ExpStmt . LitE
      -- , c1 parS
      -- Removed paralell comprehensions
      ]


instance Enumerable Pat where
  enumerate = datatype
      [ c1 LitP
      , c1 $ \(BindN n) -> VarP n
      , c1 TupP
      , c1 $ \(UpcaseName n,ps) -> ConP n ps
      , c1 $ \(p1,UpcaseName n,p2) -> InfixP p1 n p2
      , c1 TildeP
    --  , c1 $ \(LcaseN n) -> BangP $ VarP n
      , c1 $ \(BindN n,p) -> AsP n p
      , c0 WildP
      , c1 $ \(UpcaseName e,x) -> RecP e (map (\(BindN n, p) -> (n,p)) (nonEmpty x))
      , c1 ListP
    --  , c2 SigP -- BUGGY!
    --  , c2 ViewP -- BUGGY!
      ]


-- deriveEnumerable ''Match  -- Should remove decs
instance Enumerable Match where
 enumerate = datatype
      [c3 $ \x y ds -> Match x y (map unWhere ds)
      ]

instance Enumerable Stmt where
  enumerate = datatype
      [ c2 BindS
      , c1 $ \(d) -> LetS $ map unWhere $ nonEmpty d
      , c1 $ NoBindS
      -- , c1 parS
      -- Removed paralell comprehensions
      ]



instance Enumerable Name where
 enumerate = datatype [ c2 Name ]



instance Enumerable Type where
 enumerate = datatype cType where
  cType =
    [c3 $ (\(x) -> ForallT (map (PlainTV . lcased) $ nonEmpty x))
    ,c1 $ \(BindN a) -> VarT a
    ,c1 $ \(UpcaseName a) -> ConT a
    ,c1 $ \n -> TupleT (abs n)
    ,c0 ArrowT
    ,c0 ListT
    ,c2 AppT
    -- ,c2 SigT -- BUGGY!
    ]



-- deriveEnumerable ''Dec

instance Enumerable WhereDec where
  enumerate = datatype
    [ c1 $ \(n,c)  -> WhereDec $ FunD n (nonEmpty c)
    , c1 $ \(n,p,wds) -> WhereDec $ ValD n p (map unWhere wds)
    , c1 $ \(BindN a,b)     -> WhereDec $ SigD a b
    -- , c1 $ WhereDec . PragmaD -- Removed pragmas
    -- , c1 parS -- Removed paralell comprehensions
    ]




instance Enumerable Lit where
  enumerate = datatype
    [ c1 StringL
    , c1 CharL    -- TODO: Fair char generation
    , c1 $ IntegerL . nat
    -- , c1 RationalL -- BUGGY!
    -- Removed primitive litterals
    ]
  
instance Enumerable Clause where
  enumerate = datatype
    [c3 $ \ps bs ds -> Clause ps bs (map unWhere ds)]





-- deriveEnumerable ''Pred
--cPred =
--  [ c2 ClassP
--  , c2 EqualP
--  ]
--instance Enumerable Pred where
--  enumerate = datatype cPred


-- deriveEnumerable ''TyVarBndr
instance Enumerable TyVarBndr where
  enumerate = datatype
    [ c1 PlainTV
    , c2 KindedTV
    ]


--cKind =
--  [c0 StarK
--  ,c2 ArrowK
--  ]
--instance Enumerable Kind where
--  enumerate = datatype cKind


instance Enumerable Body where
 enumerate = datatype
    [ c1 NormalB
    , c1 $ \(x) -> GuardedB (nonEmpty x)
    -- Removed primitive litterals
    ]
  
instance Enumerable Guard where
 enumerate = datatype
   [c1 $ NormalG
   ,c1 $ \(s) -> PatG (nonEmpty s)
   ]

instance Enumerable Callconv where
  enumerate = datatype [c0 CCall, c0 StdCall]




instance Enumerable Safety where
  enumerate = datatype 
    [c0 Unsafe, c0 Safe, c0 Interruptible]


--cStrict = [c0 IsStrict, c0 NotStrict, c0 Unpacked]
--instance Enumerable Strict where
--  enumerate = datatype cStrict

--cInlineSpec = [c3 $ InlineSpec)]
--instance Enumerable InlineSpec where
--  enumerate = datatype cInlineSpec

instance Enumerable OccName where
  enumerate = datatype
   [ c0 $ OccName "Con"
   , c0 $ OccName "var"
   ]

instance Enumerable BindN where
  enumerate = datatype
    [c0 $ BindN $ Name (OccName "var") NameS]

instance Enumerable LcaseN where
  enumerate = datatype 
    [c1 $ \nf -> LcaseN $ Name (OccName "var") nf]

instance Enumerable UpcaseName where
  enumerate = datatype
    [c1 $ \nf -> UpcaseName $ Name (OccName "Con") nf]

instance Enumerable ModName where
  enumerate = datatype
    [c0 $ ModName "M", c0 $ ModName "C.M"]

instance Enumerable Range where
  enumerate = datatype 
    [ c1 FromR
    , c2 FromThenR
    , c2 FromToR
    , c3 FromThenToR
    ]


instance Enumerable NameFlavour where
  enumerate = datatype
    [c1 NameQ
--    , c3 NameG
--    , \(I# x) -> NameU x
--    , \(I# x) -> NameL x
    , c0 NameS
    ]


-- main = test_parsesBounded
-- or test_parsesAll, but that takes much longer to find bugs


