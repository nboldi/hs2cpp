{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module HaskellToMacro.PrettyPrint (plugin, prettyPrintBinding, prettyPrintExpression) where

import GhcPlugins
import StaticFlags

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.IORef

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  liftIO $ do ready <- readIORef v_opt_C_ready
              when (not ready) $ void $ parseStaticFlags []
  return (CoreDoPluginPass "ToMacroPass" pass : todo)

data CoreShowEnv = CoreShowEnv { dynFlags :: DynFlags, indentation :: Int }
  
type CoreShowM = ReaderT CoreShowEnv CoreM

runCoreShow :: CoreShowEnv -> CoreShowM a -> CoreM a
runCoreShow env m = runReaderT m env
  
pass :: ModGuts -> CoreM ModGuts
pass = bindsOnlyPass (mapM printBind)
  where printBind :: CoreBind -> CoreM CoreBind
        printBind bnd = do
          liftIO $ putStrLn "----"
          prettyPrintBinding bnd >>= liftIO . putStrLn
          return bnd 

prettyPrintExpression :: Expr Var -> CoreM String
prettyPrintExpression expr 
  = do df <- getDynFlags
       runCoreShow (CoreShowEnv df 0) (pretty expr)

          
prettyPrintBinding :: CoreBind -> CoreM String
prettyPrintBinding bnd 
  = do df <- getDynFlags
       runCoreShow (CoreShowEnv df 0) (pretty bnd)

class PrettyPrint t where
  prettyPrint :: t -> CoreShowM String
  prettyPrint a = indent $ do pp <- pretty a
                              i <- getIndent
                              return ("\n" ++ i ++ pp)
  pretty :: t -> CoreShowM String
  
getIndent :: CoreShowM String
getIndent = do indNum <- asks indentation
               return (concat $ replicate indNum "  ")
            
indent :: CoreShowM a -> CoreShowM a
indent = local (\s -> s { indentation = indentation s + 1 })

class ToCoreShow s where
  toCoreShow :: s -> CoreShowM String 

instance ToCoreShow String where toCoreShow = return
instance ToCoreShow (CoreShowM String) where toCoreShow = id
  
(<++>) :: (ToCoreShow a, ToCoreShow b) => a -> b -> CoreShowM String
a <++> b = (++) <$> toCoreShow a <*> toCoreShow b
          
infixr 5 <++>

instance (PrettyPrint b) => PrettyPrint (Bind b) where
  pretty (NonRec b expr) = "(NonRec" <++> prettyPrint b <++> prettyPrint expr <++> ")"
  pretty (Rec l) = "(Rec" <++> prettyBindList l <++> ")"
   where
      prettyBindList [] = return ""
      prettyBindList ((b,expr):l) = prettyPrint b <++> prettyPrint expr <++> prettyBindList l

instance (PrettyPrint b) => PrettyPrint (Expr b) where
  pretty (Var i) = "(Var " <++> pretty i <++> ")"
  pretty (Lit l) = "(Lit " <++> pretty l <++> ")"
  pretty (App expr arg) = "(App" <++> prettyPrint expr <++> prettyPrint arg <++> ")"
  pretty (Lam b expr) = "(Lam " <++> pretty b <++> prettyPrint expr <++> ")"
  pretty (Let bind expr) = "(Bind" <++> prettyPrint bind <++> prettyPrint expr <++> ")"
  pretty (Case expr name typ alts) = "(Case" <++> prettyPrint expr <++> prettyPrint name <++> prettyPrint typ <++> prettyPrint alts <++> ")"
  pretty (Cast expr coer) = "(Cast" <++> prettyPrint expr <++> "_ )"
  pretty (Tick _ expr) = "(Tick _" <++> prettyPrint expr <++> ")"
  pretty (Type typ) = "(Type " <++> pretty typ <++> ")"
  pretty (Coercion coer) = return "(Coercion _)"

instance (PrettyPrint (Alt b)) => PrettyPrint [Alt b] where
  pretty ls = foldl (<++>) (return "") $ map prettyPrint ls

instance (PrettyPrint b) => PrettyPrint (Alt b) where
  pretty (altcon, bl, expr) = "(Alt" <++> prettyPrint altcon <++> "[" <++> vars <++> "]" <++> prettyPrint expr <++> ")"
    where vars :: CoreShowM String
          vars = case varPr of [] -> return ""
                               fs:rest -> foldl (\a b -> a <++> "," <++> b) fs rest
          varPr = map pretty bl

instance PrettyPrint AltCon where
  pretty (DataAlt dc) = "(DataAlt " <++> (qualify $ dataConName dc) <++> ")"
  pretty (LitAlt l) = "(LitAlt " <++> pretty l <++> ")"
  pretty DEFAULT = return "DEFAULT"
  
instance PrettyPrint Var where
  pretty v = return $ qualify (varName v)
  
qualify :: Name -> String
qualify n = maybe "" (\mod -> moduleNameString (moduleName mod) ++ ".") (nameModule_maybe n) ++ getOccString n
  
instance PrettyPrint Literal where
  pretty (MachChar c) = return $ "(MachChar " ++ show c ++ ")"
  pretty (MachStr b) = return $ "(MachStr " ++ show b ++ ")"
  pretty (MachNullAddr) = return $ "MachNullAddr"
  pretty (MachInt i) = return $ "(MachInt " ++ show i ++ ")"
  pretty (MachInt64 i) = return $ "(MachInt64 " ++ show i ++ ")"
  pretty (MachWord w) = return $ "(MachWord " ++ show w ++ ")"
  pretty (MachWord64 w) = return $ "(MachWord64 " ++ show w ++ ")"
  pretty (MachFloat f) = return $ "(MachFloat " ++ show f ++ ")"
  pretty (MachDouble d) = return $ "(MachDouble " ++ show d ++ ")"
  pretty (MachLabel fs mi IsFunction) = return $ "(MachLabel " ++ show fs ++ " " ++ show mi ++ " IsFunction" ++ ")"
  pretty (MachLabel fs mi IsData) = return $ "(MachLabel " ++ show fs ++ " " ++ show mi ++ " IsData" ++ ")"
  pretty (LitInteger i _) = return $ "(LitInteger " ++ show i ++ ")"

instance PrettyPrint Type where
  pretty t = do dflags <- asks dynFlags
                return $ showSDoc dflags $ ppr t 

