{-#Language TemplateHaskell#-}
module Test.Feat.Internals.Derive where
import Language.Haskell.TH

-- General combinator for class derivation
instanceFor :: Name -> [[(Name,[Type])] -> Q Dec] -> Name -> Q Dec
instanceFor clname confs dtname = do
  (cxt,dtvs,cons) <- extractData dtname
  cd              <- mapM conData cons
  let 
    mkCxt = fmap (cxt++) $ mapM (classP clname . return . varT) dtvs
    mkTyp = mkInstanceType clname dtname dtvs
    mkDecs conf = conf cd

  instanceD mkCxt mkTyp (map mkDecs confs)


mkInstanceType :: Name -> Name -> [Name] -> Q Type
mkInstanceType cn dn vns = appT (conT cn) (foldl (appT) (conT dn) (map varT vns))

extractData :: Name -> Q (Cxt, [Name], [Con])
extractData n = reify n >>= \i -> return $ case i of
  TyConI (DataD cxt _ tvbs cons _)   -> (cxt, map tvbName tvbs, cons)
  TyConI (NewtypeD cxt _ tvbs con _) -> (cxt, map tvbName tvbs, [con])
  _ -> error $ "Unexpected info: " ++ show (ppr i)

tvbName :: TyVarBndr -> Name
tvbName (PlainTV n)  = n
tvbName (KindedTV n _) = n


conData :: Con -> Q (Name,[Type])
conData c = case c of
  NormalC n sts    -> return (n,map snd sts)
  RecC n vsts      -> return (n,map (\(_,s,t) -> t) vsts)
  InfixC st1 n st2 -> return (n,[snd st1,snd st2])
  ForallC _ _ c'   -> conData c'


x :: IO Type
x = runQ $ (toType ''(,)) 
  

toType n = case lookup n tups of
  Nothing -> conT n
  Just q  -> q

tups = (''(), [t|()|]):map (\(n,i) -> (n, tupleT i)) (zip [''(,), ''(,,)] [2..])