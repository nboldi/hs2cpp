{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module HaskellToMacro.MacroGen where

import GhcPlugins

import Data.Maybe
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import HaskellToMacro.Macro
import HaskellToMacro.Annotations
 
instance Show Name where
  show n = getOccString n ++ ":" ++ nameToUniqueStr n

-- * The MacroGen monad
      
-- | The monad extends the CoreM monad with functionality related to generating macros.
newtype MacroGen a = MacroGen { fromMacroGen :: ReaderT MGReadEnv (WriterT [Directive] (StateT MGState CoreM)) a }
  deriving ( Functor, Applicative, Monad, MonadState MGState
           , MonadReader MGReadEnv, MonadIO, MonadUnique, HasDynFlags )

data MGReadEnv = MGReadEnv { annots :: [(Name, [MacroAnnotation])]
                           , recCopies :: [(Name, [Name])]
                           , recDepths :: [(Name, Int)]
                           , recRewrites :: [(Name, String)]
                           , visibleVars :: [Var]
                           , applyUsage :: Int
                           }
initMGReadEnv = MGReadEnv [] [] [] [] [] 0
                                    
data MGState = MGState { uniqueId :: Int }
initMGState = MGState 0
           
instance MonadUnique m => MonadUnique (StateT s m) where
  getUniqueSupplyM = lift getUniqueSupplyM
instance (MonadUnique m, Monoid s) => MonadUnique (WriterT s m) where
  getUniqueSupplyM = lift getUniqueSupplyM
instance MonadUnique m => MonadUnique (ReaderT s m) where
  getUniqueSupplyM = lift getUniqueSupplyM   
  
instance (HasDynFlags m, Monad m) => HasDynFlags (StateT s m) where
  getDynFlags = lift getDynFlags
instance (HasDynFlags m, Monoid s, Monad m) => HasDynFlags (WriterT s m) where
  getDynFlags = lift getDynFlags
instance (HasDynFlags m, Monad m) => HasDynFlags (ReaderT s m) where
  getDynFlags = lift getDynFlags
           
-- * MacroGen operations
      
-- | Creates a MacroGen computation from a CoreM computation      
liftCoreM :: CoreM a -> MacroGen a
liftCoreM = MacroGen . lift . lift . lift
  
runMacroGen :: MacroGen a -> CoreM (a,[Directive])
runMacroGen m = evalStateT (runWriterT (runReaderT (fromMacroGen m) initMGReadEnv)) initMGState
  
-- ** Unique support
  
-- | Generates a unique name from a string.
-- Used for purely macro definitions that are not directly related to haskell artifacts.
uniqueMacroName :: String -> MacroGen String
uniqueMacroName s = MacroGen $ 
  do ui <- gets uniqueId
     modify (\s -> s { uniqueId = uniqueId s + 1 })
     return $ "HS2CPP_" ++ s ++ "_" ++ show ui
     
-- | Creates a unique name from a string using CoreM's support.
-- Used when unique names are needed for haskell-related artifacts.
uniqueName :: String -> MacroGen Name
uniqueName s 
  = do u <- getUniqueM
       return $ mkInternalName u (mkVarOcc s) noSrcSpan
  
-- | Creates a correct macro name from a unique haskell name
nameToUniqueStr :: Uniquable a => a -> String
nameToUniqueStr n = "_" ++ show (getUnique n)

-- ** Generating definitions

-- | Records a macro for output.
emitMacro :: Directive -> MacroGen ()
emitMacro m = MacroGen $ tell [m]

-- | Creates a macro with arguments for carrying the definitions in scope.
-- These arguments must be added to the calling of this macro manually.
emitLocalMacro :: String -> [String] -> String -> MacroGen ()
emitLocalMacro name args body
  = do visibleVars <- getVisibleVarNames
       emitMacro $ macro name (visibleVars ++ args) body

emitLocalParenMacro :: String -> String -> MacroGen ()
emitLocalParenMacro name body
  = do visibleVars <- getVisibleVarNames
       emitMacro $ if null visibleVars 
                     then macroWithParen name body
                     else macro name visibleVars body
       
-- | Alter the generation of a macro by adding a rewrite to it
inRecScope :: Name -> String -> MacroGen a -> MacroGen a
inRecScope orig subst 
  = local (\env -> env { recRewrites = (orig,subst) : recRewrites env })

-- | Checks if a variable should be rewritten
resolveRec :: Name -> MacroGen String
resolveRec s 
  = do scopes <- asks recRewrites 
       let sc = lookup s scopes
       case sc of Just cp -> return cp
                  Nothing -> return (nameToUniqueStr s)

-- ** Copies of recursive functions
                  
-- | Adds copies of recursive functions to alter the code generation
addRecursiveCopies :: (Name, [Name]) -> MacroGen a -> MacroGen a
addRecursiveCopies rns = local (\e -> e { recCopies = rns : recCopies e })
     
-- | Gets copies of recursive functions
getRecursiveCopies :: MacroGen [(Name, [Name])]
getRecursiveCopies = asks recCopies
     
-- | Gets copies of a given recursive function
recursiveCopiesOf :: Name -> MacroGen [Name]
recursiveCopiesOf name = asks (fromMaybe [] . lookup name . recCopies)
     
hasRecursiveCopies :: Name -> MacroGen Bool
hasRecursiveCopies name = asks (isJust . lookup name . recCopies)

-- ** Depths of recursive functions

setRecursiveDepths :: [(Name, Int)] -> MacroGen a -> MacroGen a
setRecursiveDepths depths = local (\e -> e { recDepths = depths })
     
-- | Defaults to 1 if it is not calculated
getRecDepthOf :: Name -> MacroGen Int
getRecDepthOf n = asks (fromMaybe 1 . lookup n . recDepths)

getMaxRecDepth :: MacroGen Int
getMaxRecDepth = asks (maximum . map snd . recDepths)
     
-- ** Annotations on definitions 
    
setAnnotations :: [(Name, [MacroAnnotation])] -> MacroGen a -> MacroGen a
setAnnotations ans = local (\e -> e { annots = ans })

getMacroAnnots :: MacroGen [(Name, [MacroAnnotation])]
getMacroAnnots = asks annots

getAnnotationsOf :: Name -> MacroGen [MacroAnnotation]
getAnnotationsOf name = asks (fromMaybe [] . lookup name . annots)

isExportedMacro :: Name -> MacroGen Bool
isExportedMacro name
  = do annotations <- asks annots
       let annotsOnVar = lookup name annotations
       return $ maybe True (not . null) annotsOnVar

-- ** Keeping the variables that are in scope in haskell

addVisibleVars :: [Var] -> MacroGen a -> MacroGen a
addVisibleVars vars = local (\e -> e { visibleVars = visibleVars e ++ vars })

getVisibleVarNames :: MacroGen [String]
getVisibleVarNames = map nameToUniqueStr <$> asks visibleVars

-- ** Keep usage of applications of standard functions

getApplyUsage :: MacroGen Int
getApplyUsage = asks applyUsage

applyUsed :: MacroGen a -> MacroGen a
applyUsed = local (\e -> e { applyUsage = applyUsage e + 1 })

applyUsedN :: Int -> MacroGen a -> MacroGen a
applyUsedN n = local (\e -> e { applyUsage = applyUsage e + n })

