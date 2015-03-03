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
    Safety(..), Strict(..), InlineSpec(..))
-- testing-feat
import Test.Feat
import Test.Feat.Access
import Test.Feat.Modifiers
-- template-haskell
import Language.Haskell.TH.Syntax.Internals(OccName(OccName), ModName(ModName), PkgName)
import Language.Haskell.TH.Ppr(pprint,Ppr)
-- haskell-src-meta
import Language.Haskell.Meta(toExp)
-- haskell-src-exts
import qualified Language.Haskell.Exts as E
-- quickcheck
import Test.QuickCheck hiding (NonEmpty, (><))
--base
import Data.Typeable(Typeable)
import Data.Ord
import Data.List
-- smallcheck
import Test.SmallCheck.Series hiding (Nat)
import Test.SmallCheck

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



-- Haskell parser
myParse :: String -> E.ParseResult E.Exp
myParse = E.parseWithMode E.defaultParseMode{E.extensions = 
    [ E.BangPatterns
    , E.ScopedTypeVariables
    , E.ViewPatterns
    , E.KindSignatures
    , E.ExplicitForAll
    , E.TypeFamilies
    ]}


-}

  
-- We define both SmallCheck and Feat enumerators for comparison.  
c1 :: (Serial a, Enumerable a) => (a -> b) -> (Enumerate b, Series b)
c1 f = (unary f,cons1 f)
c0 f = (nullary f, cons0 f)

instance (Serial a, Serial b) => Serial (FreePair a b) where
  series = map Free . (series >< series) 
  coseries = undefined

toSel :: [(Enumerate b, Series b)] -> Enumerate b
toSel xs = consts $ map fst xs

toSerial :: [(Enumerate b, Series b)] -> Series b
toSerial xs = foldl1 (\/) $ map snd xs



-- These statements are always expressions
newtype ExpStmt = ExpStmt Exp deriving Typeable

-- Declarations allowed in where clauses
newtype WhereDec = WhereDec{unWhere :: Dec} deriving Typeable

-- Lowecase names
newtype LcaseN = LcaseN {lcased :: Name} deriving Typeable
-- Uppercase names
newtype UpcaseName = UpcaseName {ucased :: Name} deriving Typeable
newtype BindN = BindN Name deriving Typeable


instance (Enumerable a, Serial a) => Serial (NonEmpty a) where
  series = toSerial [c1 $ NonEmpty . funcurry (:)] 
  coseries = undefined 
  
instance (Serial a, Infinite a) => Serial (Nat a) where
  series = map (\(N a) -> Nat a) . series
  coseries = undefined 


newtype CPair a b = CPair {cPair :: (a,b)} deriving Typeable

instance (Enumerable a, Serial a,Enumerable b, Serial b) => Serial (CPair a b) where
  series = toSerial [c1 $ CPair . funcurry (,)] 
  coseries = undefined 
instance (Serial a,Enumerable a,Enumerable b, Serial b) => Enumerable (CPair a b) where
  enumerate = toSel [c1 $ CPair . funcurry (,)] 

cExp =   
  [c1 $ VarE . lcased
  ,c1 $ ConE . ucased
  ,c1 LitE
  ,c1 $ funcurry AppE
  ,c1 $ \(ExpStmt a,o)   -> InfixE (Just a) (either ConE VarE o) Nothing
  ,c1 $ \(ExpStmt a,o)   -> InfixE Nothing  (either ConE VarE o) (Just a)
  ,c1 $ \(a,o,b) -> InfixE (Just a) (either ConE VarE o) (Just b)
--  ,c1 $ funcurry $ funcurry $ \a o b -> UInfixE a (VarE o) b
--  ,c1 $ funcurry $ funcurry $ \a o b -> UInfixE a (ConE o) b 
--  ,c1 ParensE
  ,c1 $ funcurry $ LamE . nonEmpty
  ,c1 $ \(x1,x2,xs) -> TupE (x1:x2:xs)
--  ,c1 UnboxedTupE
  ,c1 $ funcurry $ funcurry CondE
  ,c1 $ \(d,ds,e) -> LetE (map unWhere $ d:ds) e -- DISABLED BUGGY EMPTY LETS
  ,c1 $ \(e,NonEmpty m) -> CaseE e m
  ,c1 $ \(e,ss) -> DoE (ss ++ [NoBindS e])
  ,c1 $ (\((p,e),(CPair (xs,e'))) -> CompE ([BindS p e] ++ xs ++ [NoBindS e']))
--  ,c1 ArithSeqE -- BUGGY!
  ,c1 ListE
--  ,c1 $ funcurry SigE -- BUGGY!
  ,c1 $ \(e,x) -> RecConE e $ map unCase (nonEmpty x)
  ,c1 $ \(e,fe) -> RecUpdE e $ map unCase (nonEmpty fe)
  ]
instance Enumerable Exp where
  enumerate = toSel cExp
instance Serial Exp where
  series = toSerial cExp
  coseries = undefined

unCase (LcaseN n,e) = (n,e)

cExpStmt = 
  [ c1 $ ExpStmt . VarE
  , c1 $ ExpStmt . ConE
  , c1 $ ExpStmt . LitE
  , c1 $ \(e1,e2) -> ExpStmt (AppE e1 e2)
  , c1 $ ExpStmt . LitE
  -- , c1 parS
  -- Removed paralell comprehensions
  ]
instance Enumerable ExpStmt where
 enumerate = toSel cExpStmt
instance Serial ExpStmt where
  series = toSerial cExpStmt
  coseries = undefined
  
cPat =   
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
--  , c1 $ funcurry SigP -- BUGGY!
--  , c1 $ funcurry ViewP -- BUGGY!
  ]
instance Enumerable Pat where
 enumerate = toSel cPat
instance Serial Pat where
  series = toSerial cPat
  coseries = undefined



-- deriveEnumerable ''Match  -- Should remove decs
cMatch = 
  [c1 $ funcurry $ funcurry $ \x y ds -> Match x y (map unWhere ds)
  ]
instance Enumerable Match where
 enumerate = toSel cMatch
instance Serial Match where
  series = toSerial cMatch
  coseries = undefined  
  
cStmt = 
  [ c1 $ funcurry BindS
  , c1 $ \(d) -> LetS $ map unWhere $ nonEmpty d
  , c1 $ NoBindS
  -- , c1 parS
  -- Removed paralell comprehensions
  ]
instance Enumerable Stmt where
 enumerate = toSel cStmt
instance Serial Stmt where
  series = toSerial cStmt
  coseries = undefined


cName = [ c1 (funcurry Name) ]
instance Enumerable Name where
 enumerate = toSel cName
instance Serial Name where
  series = toSerial cName
  coseries = undefined

cType = 
  [c1 $ funcurry $ funcurry $ (\(x) -> ForallT (nonEmpty x))
  ,c1 $ \(BindN a) -> VarT a
  ,c1 $ \(UpcaseName a) -> ConT a
  ,c1 $ \n -> TupleT (abs n)
  ,c0 ArrowT
  ,c0 ListT
  ,c1 $ funcurry AppT
  -- ,c1 $ funcurry SigT -- BUGGY!
  ]
instance Enumerable Type where
 enumerate = toSel cType
instance Serial Type where
  series = toSerial cType
  coseries = undefined


-- deriveEnumerable ''Dec

cWhereDec = 
  [ c1 $ \(n,c)  -> WhereDec $ FunD n (nonEmpty c)
  , c1 $ \(n,p,wds) -> WhereDec $ ValD n p (map unWhere wds)
  , c1 $ \(BindN a,b)     -> WhereDec $ SigD a b
  -- , c1 $ WhereDec . PragmaD -- Removed pragmas
  -- , c1 parS -- Removed paralell comprehensions
  ]
instance Enumerable WhereDec where
  enumerate = toSel cWhereDec
instance Serial WhereDec where
  series = toSerial cWhereDec
  coseries = undefined


  
cLit = 
  [ c1 StringL
  , c1 CharL    -- TODO: Fair char generation
  , c1 $ IntegerL . nat
  -- , c1 RationalL -- BUGGY!
  -- Removed primitive litterals
  ]
instance Enumerable Lit where
  enumerate = toSel cLit
instance Serial Lit where
  series = toSerial cLit
  coseries = undefined


cClause = 
 [c1 $ funcurry (funcurry $ \ps bs ds -> Clause ps bs (map unWhere ds))]
instance Enumerable Clause where
  enumerate = toSel cClause
instance Serial Clause where
  series = toSerial cClause
  coseries = undefined





-- deriveEnumerable ''Pred
cPred = 
  [ c1 $ funcurry ClassP
  , c1 $ funcurry EqualP
  ]
instance Enumerable Pred where
  enumerate = toSel cPred
instance Serial Pred where
  series = toSerial cPred
  coseries = undefined

-- deriveEnumerable ''TyVarBndr
cTyVarBndr = 
  [ c1 $ PlainTV
  , c1 $ funcurry KindedTV
  ]
instance Enumerable TyVarBndr where
  enumerate = toSel cTyVarBndr
instance Serial TyVarBndr where
  series = toSerial cTyVarBndr
  coseries = undefined


cKind = 
  [c0 StarK
  ,c1 (funcurry ArrowK)
  ]
instance Enumerable Kind where
  enumerate = toSel cKind
instance Serial Kind where
  series = toSerial cKind
  coseries = undefined


cBody =
  [ c1 NormalB
  , c1 $ \(x) -> GuardedB (nonEmpty x)
  -- Removed primitive litterals
  ]
instance Enumerable Body where
 enumerate = toSel cBody
instance Serial Body where
  series = toSerial cBody
  coseries = undefined

cGuard = 
  [c1 $ NormalG
  ,c1 $ \(s) -> PatG (nonEmpty s)
  ]
instance Enumerable Guard where
 enumerate = toSel cGuard
instance Serial Guard where
  series = toSerial cGuard
  coseries = undefined
  

cCallconv = [c0 CCall, c0 StdCall]
instance Enumerable Callconv where
  enumerate = toSel cCallconv
instance Serial Callconv where
  series = toSerial cCallconv
  coseries = undefined


cSafety = [c0 Unsafe, c0 Safe, c0 Interruptible]
instance Enumerable Safety where
  enumerate = toSel cSafety
instance Serial Safety where
  series = toSerial cSafety
  coseries = undefined
  

cStrict = [c0 IsStrict, c0 NotStrict, c0 Unpacked]
instance Enumerable Strict where
  enumerate = toSel cStrict
instance Serial Strict where
  series = toSerial cStrict
  coseries = undefined

cInlineSpec = [c1 (funcurry $ funcurry $ InlineSpec)]
instance Enumerable InlineSpec where
  enumerate = toSel cInlineSpec
instance Serial InlineSpec where
  series = toSerial cInlineSpec
  coseries = undefined

cOccName = 
   [ c0 $ OccName "Con"
   , c0 $ OccName "var"
   ]
instance Enumerable OccName where
  enumerate = toSel cOccName
instance Serial OccName where
  series = toSerial cOccName
  coseries = undefined

cBindN = [c0 $ BindN $ Name (OccName "var") NameS]
instance Enumerable BindN where
  enumerate = toSel cBindN
instance Serial BindN where
  series = toSerial cBindN
  coseries = undefined
  
cLcaseN = [c1 $ \nf -> LcaseN $ Name (OccName "var") nf]
instance Enumerable LcaseN where
  enumerate = toSel cLcaseN
instance Serial LcaseN where
  series = toSerial cLcaseN
  coseries = undefined
  
cUpcaseName = [c1 $ \nf -> UpcaseName $ Name (OccName "Con") nf]
instance Serial UpcaseName where
  series = toSerial cUpcaseName
  coseries = undefined
instance Enumerable UpcaseName where
  enumerate = toSel cUpcaseName
  
cModName = [c0 $ ModName "M", c0 $ ModName "C.M"]
instance Enumerable ModName where
  enumerate = toSel cModName
instance Serial ModName where
  series = toSerial cModName
  coseries = undefined
   

cRange = 
  [ c1 FromR
  , c1 (funcurry FromThenR)
  , c1 (funcurry FromToR)
  , c1 (funcurry $ funcurry FromThenToR)
  ]
instance Enumerable Range where
  enumerate = toSel cRange
instance Serial Range where
  series = toSerial cRange
  coseries = undefined

cNameFlavour = (
  [ c1 NameQ
--    , funcurry $ funcurry NameG 
--    , \(I# x) -> NameU x
--    , \(I# x) -> NameL x
  , c0 NameS
  ])
instance Enumerable NameFlavour where
  enumerate = toSel cNameFlavour
instance Serial NameFlavour where
  series = toSerial cNameFlavour
  coseries = undefined


-- main = test_parsesBounded
-- or test_parsesAll, but that takes much longer to find bugs

eExp :: Enumerate Exp
eExp = toSel cExp


