{-# Language TemplateHaskell #-}
import Test.Feat
-- import Test.Feat.Class
import Test.Feat.Modifiers

import Control.Enumerable

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax

import Control.Exception as Ex

-- Welcome to the automatic HSE tester!
-- Things to try while youre here:
--   switch between Exp/Module/Decl etc. as testing types (e.g. TestRoundtrip)
--   to discover bugs in the various entry-points of the grammar.

-- TODOs: add some newtypes and modifiers to deal with syntax type invariants
-- (such as only enumerating non-empty do-expressions with a statement as last
-- expression).
--
-- Catalogue and report all the bugs found.


main = main_parse 100

run n = main_parse n


-- Everything which is produced by the pretty printer and is parseable is
-- semantically euivalent to the original.
type TestRoundtrip = Exp
main_round n = ioFeat (take n values) (rep_round :: TestRoundtrip -> IO())

rep_round :: (Eq a,Parseable a, Show a, Pretty a) => a -> IO ()
rep_round e = case myParse $ prettyPrint e of
  ParseOk e' -> if e == e' || prettyPrint e == prettyPrint e' then return () else do
    putStrLn $ "------ Error ------"
    putStrLn $ "e:          "++ (show  e)
    putStrLn $ "e(Pretty):  "++(prettyPrint e)
    putStrLn $ "e':         "++ (show  e')
    putStrLn $ "e'(Pretty): "++(prettyPrint e')
    putStrLn ""
  ParseFailed _ err -> return ()



-- Everything produced by the pretty printer is parseable.
type TestParse = Module
main_parse n = ioFeat (take n values) (rep_parse :: TestParse -> IO())

rep_parse :: (Parseable a, Show a, Pretty a) => a -> IO ()
rep_parse e = case myParse $ prettyPrint e of
  ParseOk e' -> const (return ()) (e' `asTypeOf` e)
  ParseFailed _ err -> do
    putStrLn (show  e)
    putStrLn (prettyPrint e)
    putStrLn err
    putStrLn ""


-- The pretty printer doesnt fail, for testing the enumerators.
type TestPrint = Module

main_print n = ioFeat (take n values) (rep_print :: TestPrint -> IO())

main_print' n = ioFeat (take n $ bounded 100000) (rep_print :: TestPrint -> IO())


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

myParse :: Parseable a => String ->  ParseResult a
myParse = parseWithMode defaultParseMode{
  extensions = ge'
  }

ge' = map EnableExtension
      [ TypeFamilies
      , TemplateHaskell
      , MagicHash
      , ParallelArrays
      , LambdaCase
      , ExplicitNamespaces
      ]


sureParse :: Parseable a => String -> a
sureParse s = case myParse s of
  ParseOk a -> a
  ParseFailed _ err -> error err

parse_print :: (Parseable a, Pretty a) => String -> (a,String)
parse_print s = let a = sureParse s in (a,prettyPrint a)






-- Uncomment the dExcluding line to enable known bugs
(let 

  buggy1 = 
    dExcluding 'UnboxedSingleCon . 
    dAll
  buggy2 =
    dExcluding 'PQuasiQuote .
    dAll 
  buggy3 = 
    dExcept 'LCase [| c1 (LCase . nonEmpty) |] .
    dExcept 'Do [| c2 $ \ss e -> Do (ss ++ [Qualifier e]) |] .
  
    dExcluding 'XPcdata .
    dExcluding 'XExpTag .
    dExcluding 'XChildTag .
    dExcept 'XPcdata [| c1 $ XPcdata . nonEmpty |] .
    dExcept 'MultiIf [| c1 $ MultiIf . nonEmpty |] .
    dAll
  
  
  fixdecs = 
    dExcept 'InfixDecl [| c4 $ \a b c -> InfixDecl a b c . nonEmpty |] .
    dAll
    
  fixlit = 
    dExcept 'PrimWord [| c1 (\x -> PrimWord (toInteger (x :: Word))) |] .
    dAll
    

 in fmap concat $ mapM deriveEnumerable' [
  dAll ''Module,
--  dAll ''SrcLoc,
  dExcluding 'AnnModulePragma $ dExcluding 'LanguagePragma
   -- dExcept 'LanguagePragma [|c2 $ \x -> LanguagePragma x . nonEmpty|] 
    $ dAll ''ModulePragma,
  dExcept 'WarnText [|c1 $ WarnText . nonEmpty|]
    $ dExcept 'DeprText [|c1 $ DeprText . nonEmpty|]  
    $ dAll ''WarningText,
  dAll ''ImportDecl,
  fixdecs ''Decl,
  dAll ''Tool,
  dAll ''QName,
  dAll ''ImportSpec,
  dAll ''Annotation,
  dAll ''Type,
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
  dAll ''Splice,
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
  dAll ''BooleanFormula
  ])

instance Enumerable ExportSpec where
  enumerate = datatype [ c1 $ EVar . nonSpecial
                       , c2 $ \x -> EAbs x . nonSpecial
                       -- , c1 $ EThingAll . nonSpecial
                       -- , c2 $ EThingWith . nonSpecial
                       , c1 $ EModuleContents
                       ]

-- newtype Upper = Upper {upper :: QName}
-- instance Enumerable Upper where
--  enumerate = datatype [c2 Qual

newtype NonSpecialName = NonSpecialName {nonSpecial :: QName}
instance Enumerable NonSpecialName where
  enumerate = datatype [fmap NonSpecialName $ c2 Qual
                       ,fmap NonSpecialName $ c1 UnQual
                       ]


instance Enumerable ModuleName where 
  enumerate = datatype 
    [ c0 $ ModuleName "M"
    , c0 $ ModuleName "C.M"
    ]

-- Will probably need to be broken into constructor/variable/symbol names
instance Enumerable Name where
  enumerate = datatype 
    [ c0 $ Ident "C"
    , c0 $ Ident "v"
--    , c0 $ Symbol "*"
    ]

instance Enumerable CName where
  enumerate = datatype
    [ c0 $ VarName (Ident "v")
    , c0 $ ConName (Ident "C")
    ]

instance Enumerable SrcLoc where
  enumerate = datatype
    [ c0 (SrcLoc "File.hs" 0 0)]
