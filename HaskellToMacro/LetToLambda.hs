{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module HaskellToMacro.LetToLambda where

import GhcPlugins

import Data.Maybe
import Control.Applicative
import Control.Monad.Writer hiding (Alt)
import Control.Monad.Reader

data LetToLambdaRdEnv = LetToLambdaRdEnv { varsInScope :: [Var]
                                   , argsForDefs :: [(Var,[Var])]
                                   }

initLetToLambdaEnv = LetToLambdaRdEnv [] []

addToScope :: [Var] -> LetToLambdaM a -> LetToLambdaM a
addToScope vars fo 
  = do vars' <- mapM transformId vars
       local (\env -> env { varsInScope = varsInScope env ++ vars' }) fo

newtype LetToLambdaM a = LetToLambdaM (ReaderT LetToLambdaRdEnv (Writer [CoreBind]) a)
  deriving (Functor, Applicative, Monad, MonadReader LetToLambdaRdEnv, MonadWriter [CoreBind])

runLetToLambda :: LetToLambdaM a -> (a,[CoreBind])
runLetToLambda (LetToLambdaM fo) = runWriter (runReaderT fo initLetToLambdaEnv)

allLetsToToplevel :: [CoreBind] -> [CoreBind]
allLetsToToplevel bnds = let (res,add) = runLetToLambda $ mapM letsToToplevelBnd bnds
                          in res ++ add

letsToToplevelBnd :: CoreBind -> LetToLambdaM CoreBind
letsToToplevelBnd (Rec ls) = Rec <$> mapM (\(v,e) -> (v,) <$> letsToToplevel e) ls
letsToToplevelBnd (NonRec n e) = NonRec n <$> letsToToplevel e

-- Transform let expressions into top-level bindings
letsToToplevel :: Expr Var -> LetToLambdaM (Expr Var)
letsToToplevel (Var v) = do 
  args <- asks (lookup v . argsForDefs)
  v' <- transformId v
  return $ foldl App (Var v') (map Var (fromMaybe [] args))
letsToToplevel (App f a) = App <$> letsToToplevel f <*> letsToToplevel a
letsToToplevel (Case e a t alts) 
  = Case <$> letsToToplevel e <*> transformId a <*> return t 
         <*> addToScope [a] (mapM letsToTopLevelAlt alts)
letsToToplevel (Lam a e) 
  = Lam <$> transformId a <*> addToScope [a] (letsToToplevel e)
letsToToplevel (Let b e) 
  = do vars <- asks varsInScope
       local (\env -> env { argsForDefs = map (,vars) (bindersOf b) ++ argsForDefs env })
         $ do b' <- letsToToplevelBnd (globalize $ addParams vars b)
              tell [b']
              addToScope (bindersOf b) (letsToToplevel e)
letsToToplevel (Cast e c) = Cast <$> letsToToplevel e <*> return c
letsToToplevel (Tick t e) = Tick t <$> letsToToplevel e
letsToToplevel e = return e

letsToTopLevelAlt :: Alt Var -> LetToLambdaM (Alt Var)
letsToTopLevelAlt (tag,args,e) 
  = (tag,,) <$> mapM transformId args <*> addToScope args (letsToToplevel e)

transformId :: Var -> LetToLambdaM Var
transformId v = do trf <- asks (isJust . lookup v . argsForDefs)
                   return $ if trf then setIdExported v else v

globalize :: CoreBind -> CoreBind
globalize (Rec ls) = Rec $ map (\(v,e) -> (setIdExported v, e)) ls
globalize (NonRec n e) = NonRec (setIdExported n) e

-- TODO: only add referenced parameters
addParams :: [Var] -> CoreBind -> CoreBind
addParams vars b
  = case b of 
      Rec ls -> Rec $ map (\(v,e) -> (modifyType v, modifyExpr e) ) ls 
      NonRec n e -> NonRec (modifyType n) (modifyExpr e)
  where modifyType :: Var -> Var
        modifyType v = v { varType = mkFunTys (map varType vars) (varType v) }
        modifyExpr :: Expr Var -> Expr Var
        modifyExpr e = mkCoreLams vars e

-- Needed to be used as a Core-to-Core pass
pass :: ModGuts -> CoreM ModGuts
pass mod = do 
  reinitializeGlobals
  bindsOnlyPass (return . allLetsToToplevel) mod
