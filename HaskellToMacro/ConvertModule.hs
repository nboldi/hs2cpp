{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module HaskellToMacro.ConvertModule where

import Control.Applicative
import Control.Monad
import Data.Maybe

import GhcPlugins
import Constants
import Class
import PrelNames

import HaskellToMacro.ConvertBindings
import HaskellToMacro.AnalyzeCore
import HaskellToMacro.Predefined
import HaskellToMacro.Macro
import HaskellToMacro.MacroGen
import HaskellToMacro.Annotations
   
-- | Creates macros from a given module
convertModule :: ModGuts -> MacroGen ()
convertModule mod = loadAnnotations mod $ do 
    mapM emitMacro boostIncludes >> separate
    mapM emitMacro (getModuleImports mod) >> separate
    classBnds <- genClassSelectorFunctions (getClassFunctions mod)
    let allBnds = mg_binds mod ++ classBnds
    allAppsUsed <- loadRecursiveDepths allBnds
                     $ addAllRecCopiesForAllBnds allBnds
                     $ foldM genBind 0 (concatMap getBindingsWithRec allBnds)
    separate
    genDataConstructors (getDataConstructors mod) >> separate
    genDataConstructors wiredInDataCtors >> separate
    defaultMacros allAppsUsed
  where genBind appLevel (isRec,n,e) = do 
          applyUsedN appLevel $ bindingToMacro isRec (n,e)
          (appLevel +) <$> appliesUsedBy (varName n)
        separate = emitMacro (Comment "") >> emitMacro (Comment "")

appliesUsedBy :: Name -> MacroGen Int
appliesUsedBy n = do
  recCopyN <- length <$> recursiveCopiesOf n
  recDepth <- getRecDepthOf n
  return ((recCopyN + 1) * recDepth)
    
-- | Load annotations on identifiers that are bound in the module
loadAnnotations :: ModGuts -> MacroGen a -> MacroGen a
loadAnnotations mod mg = 
  let boundedNames = concatMap (map fst . getBindings) 
                               (mg_binds mod)
      typeNames = map tyConName (mg_tcs mod)
   in do annots <- (++) <$> mapM (loadMacroAnnots mod) boundedNames
                        <*> mapM (loadNameAnnots mod) typeNames
         setAnnotations annots mg
  
-- | Alters the computation by setting the depths of visible bindings
loadRecursiveDepths :: [CoreBind] -> MacroGen a -> MacroGen a
loadRecursiveDepths bnds mg 
  = setRecursiveDepths (recursiveDepthsOfAllBindings bnds) mg

-- | Calculates the depths of all visible bindings
recursiveDepthsOfAllBindings :: [CoreBind] -> [(Name,Int)]
recursiveDepthsOfAllBindings bnds
  = let bindings = concatMap getBindings bnds
     in map (\(name,expr) -> (varName name, exprApplyDepth expr)) bindings
        
-- | Get variable with it's macro annotations
loadMacroAnnots :: ModGuts -> Var -> MacroGen (Name, [MacroAnnotation])
loadMacroAnnots guts b 
  = (varName b,) <$> ( (++ getAnnotsFromType (varType b)) 
                         <$> liftCoreM (annotationsOn guts (varName b)) )
  
getAnnotsFromType :: Type -> [MacroAnnotation]
getAnnotsFromType t 
  = let (args,_) = splitFunTys t
        classArgs = map getClassPredTys (filter isClassPred args)
     in catMaybes $ map (\(cls,ts) -> if qualifiedName (className cls) == Just "HaskellToMacro.Annotations.Recursive" 
	                                    then Just (GenerateRecursiveMacro (fmap fromIntegral (isNumLitTy (head ts))))
                                        else Nothing) classArgs					
  
-- | Get variable with it's macro annotations
loadNameAnnots :: ModGuts -> Name -> MacroGen (Name, [MacroAnnotation])
loadNameAnnots guts b = (b,) <$> liftCoreM (annotationsOn guts b)
  
-- | Gets data constructors defined in the module
getDataConstructors :: ModGuts -> [DataCon]
getDataConstructors mod =
  concat $ map (visibleDataCons . algTyConRhs) 
         $ filter isDataTyCon
         $ mg_tcs mod

getClassFunctions :: ModGuts -> [Class]
getClassFunctions mod =
  catMaybes 
    $ map tyConClass_maybe 
    $ mg_tcs mod
      
-- ** Add recursive copies for bindings

addAllRecCopiesForAllBnds :: [CoreBind] -> MacroGen a -> MacroGen a
addAllRecCopiesForAllBnds bnds mg = foldr addAllRecCopiesForBnd mg bnds

addAllRecCopiesForBnd :: CoreBind -> MacroGen a -> MacroGen a
addAllRecCopiesForBnd bnd@(Rec _) mg 
  = foldr (\n mg -> do recNames <- createRecNames (varName n)
                       addRecursiveCopies recNames mg) 
          mg (bindersOf bnd)
addAllRecCopiesForBnd _ mg = mg
        
-- | For definitions with GenerateRecursiveMacro without sizes, 
-- or definitions where we cannot find the size yet.
defaultRecursionLimit = 16
    
-- | Creates the recursive versions for definitions of a given name
createRecNames :: Name -> MacroGen (Name,[Name])
createRecNames name = do
    let createCopies n = (name,) <$> replicateM n (uniqueName (getOccString name))
    annots <- getMacroAnnots
    case lookup name annots of 
      Just (GenerateRecursiveMacro n : _) ->
        createCopies (fromMaybe defaultRecursionLimit n)                          
      _ -> -- TODO: search for original definition
        createCopies defaultRecursionLimit
  
-- * Convert data constructors
  
-- | Data constructors wired inside the ghc compiler
wiredInDataCtors :: [DataCon]
wiredInDataCtors 
  = [ trueDataCon, falseDataCon
    , charDataCon, doubleDataCon, floatDataCon, intDataCon, wordDataCon
    , nilDataCon, consDataCon
    , ltDataCon, eqDataCon, gtDataCon
    , unitDataCon, unboxedUnitDataCon, unboxedSingletonDataCon 
    ] ++ map (tupleCon BoxedTuple) [2..mAX_TUPLE_SIZE] 
      ++ map (tupleCon UnboxedTuple) [2..mAX_TUPLE_SIZE]
             
-- TODO: generate only for annotated constructors
genDataConstructors :: [DataCon] -> MacroGen () 
genDataConstructors ctors 
  = forM_ ctors $ \ctor -> do
      let ctorName = toMacroDefName $ getOccString (dataConName ctor) 
          ctorUniqueName = nameToUniqueStr (dataConWorkId ctor) 
          ctorTag = show (dataConTag ctor)
          ctorArgNum = length (dataConOrigArgTys ctor)
      createAPIMacro' ctorName ctorUniqueName (dataConUserType ctor)
      if ctorArgNum > 0 then do
        ctorTuple <- uniqueMacroName ("CTOR_" ++ ctorName)
        let tupleMacroArgs = map (("a"++) . show) [1..ctorArgNum]
        emitMacro $ macro ctorTuple tupleMacroArgs $ toValue $ toTuple [ctorTag, toSeq tupleMacroArgs]
        emitMacro $ macro ctorUniqueName [] $ toThunk ctorTuple ctorArgNum
      else
        emitMacro $ macro ctorUniqueName [] $ toValue (toTuple [ctorTag, mSeqNil])
           
genClassSelectorFunctions :: [Class] -> MacroGen [CoreBind]
genClassSelectorFunctions cls
    = concat <$> mapM genBind cls
  where genBind :: Class -> MacroGen [CoreBind] 
        genBind cl = mapM (genMethod (classTyCon cl)) (zip [0..] (classMethods cl))
        genMethod :: TyCon -> (Int,Id) -> MacroGen CoreBind
        genMethod tc (n,id) 
          = do -- FIXME: pattern match fail on classes with one function
               let Just dc = isDataProductTyCon_maybe tc
               [v,ds,f] <- mapM makeVar [ ("v", dataConOrigResTy dc)
                                        , ("ds", dataConOrigResTy dc)
                                        , ("f", dataConOrigResTy dc) ]
               let alts = [ ( DataAlt dc
                            , updateNth n f (map wild (dataConRepArgTys dc))
                            , Var f ) ]
               return $ NonRec id (Lam v (Case (Var v) ds (varType v) alts))
        makeVar :: (String, Type) -> MacroGen Id
        makeVar (s,t) = do
          n <- uniqueName s
          return $ mkLocalVar VanillaId n t vanillaIdInfo
        wild t = mkLocalVar VanillaId wildCardName t vanillaIdInfo
        updateNth i e ls = case splitAt i ls of
          (bef, _ : aft) -> bef ++ e : aft
          (bef, [])      -> bef
           
-- * Convert imports 

-- | Generate include directives from imports in a haskell module
getModuleImports :: ModGuts -> [Directive]
getModuleImports mod 
  = map (Include . macroFileName . moduleName) 
      $ filter ((== mainPackageKey) . modulePackageKey) 
      $ moduleEnvKeys (mg_dir_imps mod)
      
        
-- | The name of the file that contains the macros generated from the given module.
macroFileName :: ModuleName -> FilePath
macroFileName = (++".h") . moduleNameSlashes