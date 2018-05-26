{-# Language TemplateHaskell #-}
import Test.Feat
-- import Test.Feat.Class
import Test.Feat.Modifiers
import Test.Feat.Driver

import Control.Enumerable

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax

import Control.Exception as Ex

-- Welcome to the automatic HSE tester!
-- Things to try while youre here:
--   switch between Exp/Module/Decl etc. as TestParse
--   to discover bugs in the various entry-points of the grammar.

-- TODOs: add some newtypes and modifiers to deal with syntax type invariants
-- (such as only enumerating non-empty do-expressions with a statement as last expression).
--
-- Catalogue and report all the bugs found.
--
-- Fix the round-trip


main = do 
  main_parse -- Test the parsable property
  main_round -- Test the round-trip property

-- Everything produced by the pretty printer is parseable.
type TestParse = Exp SrcSpanInfo -- Type to be tested
main_parse = do 
   res <- test prop_parse
   mapM (putStr . rep_parse) res


rep_parse :: TestParse -> String
rep_parse e = case myParse $ prettyPrint e of
  ParseOk e' -> const "" (e' `asTypeOf` e)
  ParseFailed _ err -> unlines
    [(show $ fmap (const ()) e)
    ,(prettyPrint e)
    ,err]

prop_parse :: TestParse -> Bool
prop_parse e = case myParse $ prettyPrint e of
  ParseOk e' -> const True (e' `asTypeOf` e)
  ParseFailed _ err -> False


-- Everything which is produced by the pretty printer parses back to itself
type TestRoundtrip = Exp SrcSpanInfo
main_round = do
   res <- testOptions defOptions{oBounded = (Just 10000)} prop_trip
   mapM (putStr . rep_trip) res

-- Tests a "roundtrip and a half"
--   parse (print e) == parse (print (parse (print e))) 
-- Ignores parsing errors
prop_trip :: TestRoundtrip -> Bool
prop_trip e = case roundAndAhalf e of
  ParseOk (e', e'') -> eq e' e''
  ParseFailed _ err -> True

eq :: TestRoundtrip -> TestRoundtrip -> Bool
eq a b = fmap (const ()) a == fmap (const ()) b


rep_trip :: TestRoundtrip -> String
rep_trip e = case roundAndAhalf e of
  ParseOk (e', e'') -> unlines [
    "Original:",
    show (fmap (const ()) e),
    prettyPrint e,
    "",
    "One print/parse round:",
    show (fmap (const ()) e'),
    prettyPrint e',
    "",
    "Two print/parse round:",
    show (fmap (const ()) e''),
    prettyPrint e''
    ]
  ParseFailed _ err -> unlines
    [(show $ fmap (const ()) e)
    ,(prettyPrint e)
    ,err]


roundAndAhalf :: TestRoundtrip -> ParseResult (TestRoundtrip, TestRoundtrip)
roundAndAhalf e = do
  e' <- myParse $ prettyPrint e
  e'' <- myParse $ prettyPrint e'
  return (e', e'')
  
{-

-- Everything which is produced by the pretty printer and is parseable is
-- semantically euivalent to the original.



-- The pretty printer doesnt fail, for testing the enumerators.
type TestPrint = Module


prop_print :: Pretty a => a -> Bool
prop_print e = length (prettyPrint e) >= 0

rep_print :: (Show a, Pretty a) => a -> IO ()
rep_print e = Ex.catch
  (prop_print e `seq` return ())
  (\err -> do
    putStrLn (show  e)
    if show (err::SomeException) == "user interrupt" then undefined else return ()
    putStrLn $ show (err::SomeException)
    putStrLn "")
-}

-- Parse with all extensions
myParse :: Parseable a => String ->  ParseResult a
myParse = parseWithMode defaultParseMode{
  extensions = [ e |e@(EnableExtension _) <- knownExtensions]
  }

sureParse :: Parseable a => String -> a
sureParse s = case myParse s of
  ParseOk a -> a
  ParseFailed _ err -> error err

parse_print :: (Parseable a, Pretty a) => String -> (a,String)
parse_print s = let a = sureParse s in (a,prettyPrint a)


instance Enumerable SrcSpanInfo where
  enumerate = datatype [c0 $ SrcSpanInfo (SrcSpan "M.hs" 0 0 0 0) []]

-- Uncomment the dExcluding line to enable known bugs
(let 

  buggy1 = 
    dExcluding 'UnboxedSingleCon . 
    dAll
  buggy2 =
    dExcluding 'PQuasiQuote .
    dAll 
  buggy3 = 
    dExcept 'Tuple [| c3 (\a b c -> Tuple a b (nonEmpty c))|] .
    dExcept 'TupleSection [| c3 (\a b c -> TupleSection a b (nonEmpty c))|] .
    dExcept 'LCase [| c2 (\a -> LCase a . nonEmpty) |] .
    dExcept 'Do [| c3 $ \a ss e -> Do a (ss ++ [Qualifier a e]) |] .
    dExcept 'Var [| c2 (\a b -> Var a (nonSpecial b)) |] .
    dExcept 'Con [| c2 (\a b -> Con a (nonSpecial b)) |] .
    
    dExcept 'Lambda [| c3 (\a b c -> Lambda a (nonEmpty b) c) |] .
    
    dExcept 'RecUpdate [| c3 (\a b c -> RecUpdate a b (nonEmpty c)) |] .
    
    dExcept 'ListComp [| c3 (\a b c -> ListComp a b (nonEmpty c)) |] .
    
    dExcluding 'TypeApp .
    dExcluding 'GenPragma . -- Seems genuinly buggy
    dExcluding 'TypQuote .
    dExcluding 'VarQuote .
    dExcluding 'BracketExp .
    dExcluding 'MDo .
    dExcluding 'IPVar . -- What is this?
    dExcluding 'QuasiQuote .
    dExcluding 'XPcdata .
    dExcluding 'XExpTag .
    dExcluding 'XChildTag .
    dExcluding 'OverloadedLabel . 
    dExcept 'XPcdata [| c2 $ \a -> XPcdata a . nonEmpty |] .
    dExcept 'MultiIf [| c2 $ \a -> MultiIf a . nonEmpty |] .
    dAll
  
  
  fixdecs = 
    dExcluding 'DeprPragmaDecl .
    dExcluding 'WarnPragmaDecl .
    dExcluding 'SpliceDecl .
    dExcept 'TypeSig [| c3 (\a b c -> TypeSig a (nonEmpty b) c) |] . 
    dExcept 'InfixDecl [| c4 $ \a b c -> InfixDecl a b c . nonEmpty |] .
    dExcept 'CompletePragma [| c3 $ \a b c -> CompletePragma a (nonEmpty b) c|] .
    dAll
    
  fixlit = 
    dExcluding 'PrimWord . 
    dExcluding 'PrimInt . -- Buggy?
    --dExcept 'PrimWord [| c2 (\a x -> PrimWord a (toInteger (x :: Word)) (show x)) |] .
    dExcept 'Int [| c2 (\a x -> Int a (toInteger (x :: Word)) (show x)) |] .
    dAll
    
  fixType = 
    dExcept 'TyUnboxedSum [| c2 (\a b -> TyUnboxedSum a (nonEmpty b))|] .
    
    dExcluding 'TyQuasiQuote .
    dAll

 in fmap concat $ mapM deriveEnumerable' [
  dExcluding 'XmlHybrid $ dExcluding 'XmlPage $ dAll ''Module,
--  dAll ''SrcLoc,
  dExcluding 'AnnModulePragma $ dExcluding 'LanguagePragma
   -- dExcept 'LanguagePragma [|c2 $ \x -> LanguagePragma x . nonEmpty|] 
    $ dAll ''ModulePragma,
  dAll ''ImportDecl,
  fixdecs ''Decl,
  dAll ''Tool,
  dAll ''QName,
  dAll ''ImportSpec,
  dAll ''Annotation,
  fixType ''Type,
  dAll ''Activation,
  dAll ''Rule,
  dAll ''CallConv,
  dAll ''Safety,
  buggy2 ''Pat,
  dAll ''Rhs,
  dAll ''Binds,
  dAll ''Match,
  buggy3 ''Exp,
  dAll ''Assoc,
  dAll ''Op,
  dAll ''Asst,
  dAll ''InstDecl,
  dAll ''TyVarBind,
  dAll ''FunDep,
  dAll ''ClassDecl,
  dAll ''DataOrNew,
  dAll ''Kind,
  dAll ''GadtDecl,
  dAll ''QualConDecl,
  buggy1 ''SpecialCon,
  dAll ''Boxed,
  dAll ''RuleVar,
  dAll ''XName,
  dAll ''PXAttr,
  dAll ''RPat,
  dAll ''PatField,
  dAll ''GuardedRhs,
  dAll ''IPBind,
  dAll ''XAttr,
  dExcluding 'IdSplice $ dAll ''Splice,
  dAll ''Bracket,
  dAll ''QualStmt,
  dAll ''FieldUpdate,
  dAll ''QOp,
  dAll ''Stmt,
  dAll ''Alt,
  fixlit ''Literal,
  dAll ''IPName,
  dAll ''ConDecl,
  dAll ''RPatOp,
  -- dAll ''GuardedAlts,
  dAll ''BangType,
  -- dAll ''GuardedAlt,
  dAll ''TypeEqn,
  dAll ''Sign,
  dAll ''Role,
  dAll ''Promoted,
  dAll ''PatternSynDirection,
  dAll ''Overlap,
  dAll ''Namespace,
  dAll ''BooleanFormula,
  dAll ''ImportSpecList,
  dAll ''DeclHead,
  dAll ''ResultSig,
  dAll ''InjectivityInfo,
  dAll ''Context,
  dAll ''Deriving,
  dAll ''InstRule,
  dAll ''Unpackedness,
  dAll ''FieldDecl, 
  dAll ''InstHead,
  dAll ''DerivStrategy,
  dAll ''MaybePromotedName
  ])



instance Enumerable a => Enumerable (ModuleHead a) where
  enumerate = datatype [c1 $ \a -> ModuleHead a (ModuleName a "M") Nothing Nothing]

instance Enumerable a => Enumerable (ExportSpec a) where
  enumerate = datatype [ c2 $ \a -> EVar a . nonSpecial
                       , c3 $ \a x -> EAbs a x . nonSpecial
                       -- , c1 $ EThingAll . nonSpecial
                       -- , c2 $ EThingWith . nonSpecial
                       , c2 $ EModuleContents
                       ]

-- newtype Upper = Upper {upper :: QName}
-- instance Enumerable Upper where
--  enumerate = datatype [c2 Qual

newtype NonSpecialName a = NonSpecialName {nonSpecial :: QName a}
instance Enumerable a => Enumerable (NonSpecialName a) where
  enumerate = datatype [fmap NonSpecialName $ c3 Qual
                       ,fmap NonSpecialName $ c2 UnQual
                       ]


instance Enumerable a => Enumerable (ModuleName a) where 
  enumerate = datatype 
    [ c1 $ \a -> ModuleName a "M"
    , c1 $ \a -> ModuleName a "C.M"
    ]

-- Will probably need to be broken into constructor/variable/symbol names
instance Enumerable a => Enumerable (Name a) where
  enumerate = datatype 
    [ c1 $ \a -> Ident a "C"
    , c1 $ \a -> Ident a "v"
--    , c0 $ Symbol "*"
    ]

instance Enumerable a => Enumerable (CName a) where
  enumerate = datatype
    [ c1 $ \a -> VarName a (Ident a "v")
    , c1 $ \a -> ConName a (Ident a "C")
    ]

instance Enumerable SrcLoc where
  enumerate = datatype
    [ c0 (SrcLoc "File.hs" 0 0)]