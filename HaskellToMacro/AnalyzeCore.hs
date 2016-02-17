module HaskellToMacro.AnalyzeCore where 

import GhcPlugins

import Data.Data (Data(..))

class QualifiedName n where
  qualifiedName :: n -> Maybe String
  
instance QualifiedName Name where
  qualifiedName = Just . getQualifiedName
  
instance QualifiedName Var where
  qualifiedName = qualifiedName . varName
  
instance QualifiedName Type where
  qualifiedName t 
    = do tc <- tyConAppTyCon_maybe t  
         qualifiedName $ tyConName tc

getQualifiedName :: Name -> String
getQualifiedName n = case nameModule_maybe n of 
  Just mod -> moduleNameString (moduleName mod) ++ "." ++ getOccString n
  Nothing -> getOccString n
  
-- | Get name-expression pairs from a (possibly recursive) binding
getBindings :: Bind Var -> [(Var, Expr Var)]
getBindings (NonRec n e) = [(n,e)]
getBindings (Rec l) = l

-- | Get recursive-name-expression triples from a binding
getBindingsWithRec :: Bind Var -> [(Bool, Var, Expr Var)]
getBindingsWithRec (NonRec n e) = [(False,n,e)]
getBindingsWithRec (Rec l) = map (\(a,b) -> (True,a,b)) l

-- | How many levels of macro application are needed to successfully 
-- evaluate this expression (without calls of recursive functions)?
exprApplyDepth :: Expr Var -> Int
exprApplyDepth (Var id) = 1
exprApplyDepth (Lam _ expr) = exprApplyDepth expr + 1
exprApplyDepth (Case expr _ _ alts) 
  = exprApplyDepth expr 
      `max` maxMap (\(_,_,e) -> exprApplyDepth e + 1) alts
exprApplyDepth (App fun arg) 
  = exprApplyDepth fun `max` exprApplyDepth arg
exprApplyDepth (Let localBnds expr) 
  = exprApplyDepth expr 
      `max` maxMap (exprApplyDepth . snd) (getBindings localBnds)
exprApplyDepth _ = 1 

maxMap :: Ord b => (a -> b) -> [a] -> b
maxMap f = maximum . map f

isRecursive :: Bind Var -> Bool
isRecursive (Rec _) = True
isRecursive (NonRec _ _) = False
         
-- | Get annotations on a name
annotationsOn :: Data a => ModGuts -> Name -> CoreM [a]
annotationsOn guts name = do
  anns <- getAnnotations deserializeWithData guts
  return $ lookupWithDefaultUFM anns [] (nameUnique name)