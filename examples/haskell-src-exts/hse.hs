{-# Language TemplateHaskell #-}
import Test.Feat
import Test.Feat.Class
import Test.Feat.Modifiers

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

ge' = [ TypeFamilies
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
    dExcluding 'XPcdata .
    dExcluding 'XExpTag .
    dExcluding 'XChildTag .
    dExcept 'XPcdata [| unary $ XPcdata . nonEmpty |] . dAll
  

 in fmap concat $ mapM deriveEnumerable' [
  dAll ''Module,
--  dAll ''SrcLoc,
  dExcept 'LanguagePragma [|unary $ funcurry $ \x -> LanguagePragma x . nonEmpty|] 
    $ dAll ''ModulePragma,
  dExcept 'WarnText [|unary $ WarnText . nonEmpty|]
    $ dExcept 'DeprText [|unary $ DeprText . nonEmpty|]  
    $ dAll ''WarningText,
  dAll ''ExportSpec,
  dAll ''ImportDecl,
  dAll ''Decl,
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
  dAll ''Literal,
  dAll ''IPName,
  dAll ''ConDecl,
  dAll ''RPatOp,
  dAll ''GuardedAlts,
  dAll ''BangType,
  dAll ''GuardedAlt
  ])



instance Enumerable ModuleName where 
  enumerate = consts 
    [ nullary $ ModuleName "M"
    , nullary $ ModuleName "C.M"
    ]

-- Will probably need to be broken into constructor/variable/symbol names
instance Enumerable Name where
  enumerate = consts 
    [ nullary $ Ident "C"
    , nullary $ Ident "v"
    , nullary $ Symbol "*"
    ]

instance Enumerable CName where
  enumerate = consts
    [ nullary $ VarName (Ident "v")
    , nullary $ ConName (Ident "C")
    ]

instance Enumerable SrcLoc where
  enumerate = consts
    [ nullary (SrcLoc "File.hs" 0 0)]





