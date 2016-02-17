{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables #-}
module HaskellToMacro.ConvertBindings where

import GhcPlugins hiding (getAnnotations)
import Unique

import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as BS

import HaskellToMacro.AnalyzeCore
import HaskellToMacro.Predefined
import HaskellToMacro.MacroGen
import HaskellToMacro.Macro
import HaskellToMacro.Transform
import HaskellToMacro.PrettyPrint

-- * Convert bindings

-- | Generates a macro for a recursive or nonrecursive binding
bindingToMacro :: Bool -> (Var, Expr Var) -> MacroGen ()
bindingToMacro recursive (var@(varName -> name), expr) = do
    emitMacro (Comment "")
    annots <- getAnnotationsOf name

    -- Debugging of simplification
    -- liftIO $ putStrLn ("--- \nSimplification of " ++ getOccString name ++ ": ")
    -- liftCoreM (prettyPrintExpression (simplificationRewrite expr) >>= liftIO . putStrLn)

    if recursive
      then do copies <- recursiveCopiesOf name
              createMacroRec var copies expr
      else do createMacro name expr
              createAPIMacro var

-- | Creates a series of macros for a recursive binding. The 'name' must be a key in 'recs'.
createMacroRec :: Var -> [Name] -> Expr Var -> MacroGen ()
createMacroRec var@(varName -> name) copies expr = do
    recs <- getRecursiveCopies
    recDepthStep <- getMaxRecDepth
    let instName n i
          = maybe (nameToUniqueStr n)
                  (\ls -> if length ls <= i then toException "RECURSION_LIMIT_REACHED"
                                            else nameToUniqueStr (ls !! i))
                  (lookup n recs)
        recDefs = map fst recs
        allRecDefsToScope i mg
          = foldr (\n -> inRecScope n (instName n i))
                  mg recDefs
    allRecDefsToScope 0 $ do createMacro name expr
                             createAPIMacro var
    sequence_ $ zipWith (applyUsedN . (*recDepthStep)) [1..] $ map
      (\i -> (allRecDefsToScope (i+1) (createMacro (copies !! i) expr)))
      [0..length copies - 1]

-- | Creates a public call for a binding, with the original name.
createAPIMacro :: Var -> MacroGen ()
createAPIMacro var@(varName -> n) = createAPIMacro' (getOccString n) (nameToUniqueStr n) (varType var)

-- | Creates a public call for a binding, with the original name.
createAPIMacro' :: String -> String -> Type -> MacroGen ()
createAPIMacro' pubName uniqName t
  = do -- show type information in a comment
       dflags <- getDynFlags
       emitMacro (Comment (pubName ++ "[" ++ uniqName ++ "]: " ++ (showSDoc dflags $ ppr t)))

       let argWrappers = getArgumentWrappers t
           unwrapper   = getResultUnwrapper t
       args <- replicateM (length argWrappers)
                 $ (nameToUniqueStr <$> uniqueName ("a"))
       emitLocalMacro pubName args
         =<< unwrapper =<< genAppliedBody uniqName (zipWith ($) argWrappers args)
  where genAppliedBody f args
          = foldl (\f' a -> mApply f' a) (return f) args



getArgumentWrappers :: Type -> [(String -> MacroGen String)]
getArgumentWrappers t = map argumentAutoBox (fst $ splitFunTys t)
  where -- FIXME: walkaround while we don't check Num instances
        argumentAutoBox t | Just tc <- tyConAppTyCon_maybe t
                          , getQualifiedName (tyConName tc) == "GHC.Types.Int"
                          = return . \v -> "(HS2CPP_VALUE)((1,(" ++ toValue v ++ ")))"
                          | Just tc <- tyConAppTyCon_maybe t
                          , getQualifiedName (tyConName tc) == "HaskellToMacro.Tokens.TokenList"
                          = return . toValue <=< parenM . return
        argumentAutoBox t | isPrimitiveType t = return . toValue
        argumentAutoBox t | otherwise = return

getResultUnwrapper :: Type -> (String -> MacroGen String)
getResultUnwrapper t
  -- FIXME: walkaround while we don't check Num instances
  | Just tc <- tyConAppTyCon_maybe (getResultType t)
  , getQualifiedName (tyConName tc) == "GHC.Types.Int"
  = unwrapIfValue (mGetVal <=< mSeqElem 0 . mGetADTArgs)
  | Just tc <- tyConAppTyCon_maybe (getResultType t)
  , getQualifiedName (tyConName tc) == "HaskellToMacro.Tokens.TokenList"
  = unwrapIfValue mRemoveParen
  | isPrimitiveType (getResultType t)
  = unwrapIfValue id
  | otherwise = return
  where getResultType t | isFunTy t = snd $ splitFunTys t
                        | otherwise = t

unwrapIfValue :: (MacroGen String -> MacroGen String) -> String -> MacroGen String
unwrapIfValue ifValue s 
  = mIf (mIsException (return s)) (return s) 
                      (toSeqM [mSeqElem 0 (return s), ifValue (mGetVal s)])
                        
-- | Creates a macro for a binding with a unique name
createMacro :: Name -> Expr Var -> MacroGen ()
createMacro v e
  = exprToMacro (simplificationRewrite e)
      >>= emitLocalMacro (nameToUniqueStr v) []

-- * Convert expressions

-- | Creates a macro body from an expression,
-- using direct substitution if possible
exprToMacro :: Expr Var -> MacroGen String
exprToMacro b
  = do replacedDef <- runMaybeT $ 
         case b of Var v | Just qname <- qualifiedName v
                         -> replaceDefinition qname     
                   _ -> mzero
       case replacedDef of
         Just x -> return x
         Nothing -> do
           val <- runMaybeT (specialCase b) 
           case val of Just m -> return m
                       Nothing -> exprToMacro' b

specialCase :: Expr Var -> MaybeT MacroGen String

-- replace calls to tokenize function
specialCase (App (App (Var (qualifiedName -> Just "HaskellToMacro.Tokens.tokenize")) 
                      (Var (qualifiedName -> Just "HaskellToMacro.Tokens.$fTokensInt"))) e)
  = do eVal <- lift $ exprToMacro' e
       lift $ (toValue <$> parenM (mGetVal =<< mSeqElem 0 (mGetADTArgs (mGetVal eVal))))
       
-- insert macro token sequences
specialCase 
  (App (App (Var (qualifiedName -> Just "Data.String.fromString")) 
            (Var (qualifiedName -> Just "HaskellToMacro.Tokens.$fIsStringTokenList"))) 
       (Lit (MachStr litStr)))
  = return $ toValue $ "(" ++ BS.unpack litStr ++ ")"

-- replace error calls with exceptions
specialCase (App (Var (qualifiedName -> qn)) (Lit (MachStr str)))
  | qn `elem` map Just internalErrors
  = return (toException $ BS.unpack str)
  
specialCase _ = mzero

internalErrors :: [String]
internalErrors 
  = map ("Control.Exception.Base." ++)
        [ "recSelError", "runtimeError", "absentError", "nonExhaustiveGuardsError"
        , "irrefutPatError", "recConError", "noMethodBindingError", "patError"]
       ++ ["GHC.Err.error"]

-- | Creates a macro body from an expression
exprToMacro' :: Expr Var -> MacroGen String
exprToMacro' (Var id)  = resolveRec (varName id)
exprToMacro' (Lit lit) = litToMacro lit
exprToMacro' (App e a)
  = do expr <- exprToMacro e
       arg <- exprToMacro a
       mApply (return expr) (return arg)
-- TODO: only add original, used variables to the visible variables
exprToMacro' e@(Lam _ _)
  = do let (binders,innerE) = collectBinders e
       visibleVars <- getVisibleVarNames
       addVisibleVars binders $
         do bodyName <- uniqueMacroName "LAM_BODY"
            bodyBody <- applyUsed $ exprToMacro innerE
            emitLocalMacro bodyName [] bodyBody
            return $ toPartlyAppliedThunk bodyName (length binders) visibleVars
exprToMacro' (Let _ _) = error "exprToMacro': Let expression found in input"

exprToMacro' (Case e _ _ alts) = patternMatch e alts
exprToMacro' (Cast e _) = exprToMacro e
exprToMacro' (Tick _ e) = exprToMacro e
exprToMacro' (Type _) = return ""
exprToMacro' (Coercion _) = error "Trying to create macro from coercion"

-- | Creates a value from a literal
litToMacro :: Literal -> MacroGen String
litToMacro (MachChar c) = return $ toValue [c]
litToMacro (MachStr bs) = return $ toMacroString $ BS.unpack bs
litToMacro MachNullAddr = error "trying to convert null address"
litToMacro (MachInt i) = return $ toValue (show i)
litToMacro (MachInt64 i) = return $ toValue (show i)
litToMacro (MachWord i) = return $ toValue (show i)
litToMacro (MachWord64 i) = return $ toValue (show i)
litToMacro (MachFloat _) = error "floating literals are not supported"
litToMacro (MachDouble _) = error "floating literals are not supported"
litToMacro (MachLabel _ _ _) = error "label literals are not supported"
litToMacro (LitInteger i _) = return $ toValue (show i)

-- | Generates code for a pattern match
patternMatch :: Expr Var -> [Alt Var] -> MacroGen String
patternMatch e branches
  = do let tag = if litCase then mGetVal "t" else mGetADTTag (mGetVal "t")
           args = if litCase then return [] else mGetADTArgs (mGetVal "t")
       (altPrefix, suffixes) <- alternativeMatches litCase concreteAlts
       dispatcherName <- dispatchOnTag tag args altPrefix
       defCheckName <- checkDefaultCase defaultAlt dispatcherName tag suffixes
       visibleVars <- getVisibleVarNames
       tryMatchedExpr e (fromMaybe dispatcherName defCheckName)
    where (concreteAlts, defaultAlt) = findDefault branches
          litCase = any (\(tag,_,_) -> case tag of LitAlt _ -> True; _ -> False) concreteAlts

-- | Checks if the expression that is matched is an exception.
-- If so, returns the exceptions otherwise applies the next stage.
tryMatchedExpr :: Expr Var -> String -> MacroGen String
tryMatchedExpr e nextStage = do
  visibleVars <- getVisibleVarNames
  expr <- exprToMacro e
  (if length visibleVars > 0
     then mTryCtx (return nextStage) (return expr) (map return visibleVars)
     else mTry (return nextStage) (return expr))

-- | Checks if the argument is of the default case and
-- Generates code for the default case
checkDefaultCase :: Maybe (Expr Var) -> String -> MacroGen String -> [String] -> MacroGen (Maybe String)
checkDefaultCase (Just def) dispatcherName tag sufs = do
  visibleVars <- getVisibleVarNames
  defCheckName <- uniqueMacroName "CASE_DEF_CHECK"
  defExecName <- uniqueMacroName "CASE_DEF_EXEC"
  emitLocalMacro defCheckName ["t"]
    =<< mIf (mListMember tag (return $ toList sufs))
            (mApp dispatcherName (map return (visibleVars ++ ["t"])))
            (mApplyWhen (mNot (mListMember tag (return $ toList sufs)))
                        (return defExecName) (return (toTuple visibleVars)))
  applyUsed $ emitLocalParenMacro defExecName =<< exprToMacro def
  return (Just defCheckName)
checkDefaultCase Nothing _ _ _ = return Nothing

-- | Generates a macro to decide based on the tag, which alternative
-- is present.
dispatchOnTag :: MacroGen String -> MacroGen String -> String -> MacroGen String
dispatchOnTag tag args altPrefix = do
  visibleVars <- getVisibleVarNames
  dispatcherName <- uniqueMacroName "CASE_DISPATCH"
  emitLocalMacro dispatcherName ["t"]
    =<< ((return (altPrefix ++ "_")) `mConcat` tag)
          <++> mSeqToTuple (mSeqAppend (return $ toSeq visibleVars) args)
  return dispatcherName

-- Generate macros for each pattern alternative
alternativeMatches :: Bool -> [Alt Var] -> MacroGen (String,[String])
alternativeMatches litCase alts = do
    altPrefix <- uniqueMacroName "CASE_ALT"
    let completeAltPrefix = altPrefix ++ "_"
    mapM_ (\(tag,args,e) -> applyUsed $ addVisibleVars args $ do
              body <- exprToMacro e
              emitLocalMacro (completeAltPrefix ++ idForTag tag) [] body)
          alts
    return (altPrefix, sufs)
  where sufs = map (\(tag,_,_) -> idForTag tag) alts
        idForTag (DataAlt con) = show (dataConTag con)
        idForTag (LitAlt lit) = litId lit

-- | Get a unique identifier for a literal
litId :: Literal -> String
litId (MachChar c) = toMacroDefName [c]
litId (MachStr bs) = toMacroDefName (BS.unpack bs)
litId MachNullAddr = error "trying to convert null address"
litId (MachInt i) = show i
litId (MachInt64 i) = show i
litId (MachWord i) = show i
litId (MachWord64 i) = show i
litId (MachFloat _) = error "floating literals are not supported"
litId (MachDouble _) = error "floating literals are not supported"
litId (MachLabel _ _ _) = error "label literals are not supported"
litId (LitInteger i _) = show i

toMacroString :: String -> String
toMacroString (c:rest) = toValue (toTuple [getDataConTag consDataCon, toList [[c], toMacroString rest]])
toMacroString [] = toValue (toTuple [getDataConTag nilDataCon, toList []])

getDataConTag :: DataCon -> String
getDataConTag = nameToUniqueStr . dataConName
