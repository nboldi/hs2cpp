{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module HaskellToMacro.Transform where

-- TODO: replace simple name checks with unique-based checks 
-- or qualified package and module name checks
import GhcPlugins
-- import PrelNames

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data ()
import qualified Data.ByteString.Char8 as BS

import HaskellToMacro.AnalyzeCore
import HaskellToMacro.Predefined
import HaskellToMacro.MacroGen


-- | Deeply simplifies the expression until no 
-- additional simplification can be performed
simplificationRewrite :: Expr Var -> Expr Var
simplificationRewrite = rewrite simplifyExpr . transform normalizeStringLiterals

-- | Simplifies the expression by removing all 
-- details not necessary for macro generation.
simplifyExpr :: Expr Var -> Maybe (Expr Var)

-- Remove unnecessary function applications
simplifyExpr (App (Lam _ e) (Var id)) | getOccString id == "void#" 
  = Just e
simplifyExpr (App (Var e) a) | getOccString e `elem` ignoredApp
  = Just a
  
-- Remove unnecessary case expressions
simplifyExpr (Case e v _ [(DEFAULT,[],(App f (Var id)))]) 
  | getOccString v == "wild"
    && getOccString id == "wild"
  = Just (App f e)
  
-- Remove type applications and abstractions
simplifyExpr (App e (Type _)) = Just e
simplifyExpr (Lam a e) | isTyVar a
  = Just e
simplifyExpr (App e (Var id)) | isTyVar id 
  = Just e
  
simplifyExpr _ = Nothing

-- | Get every string literal to the (Lit (MachStr str)) form
normalizeStringLiterals :: Expr Var -> Expr Var
normalizeStringLiterals (App (App (App (Var (qualifiedName -> Just "GHC.Types.:"))
                            (Type (qualifiedName -> Just "GHC.Types.Char")))
                       (App (Var (qualifiedName -> Just "GHC.Types.C#"))
                            (Lit (MachChar ch))))
                  (Lit (MachStr str)))
  = Lit (MachStr (ch `BS.cons` str))
normalizeStringLiterals (App (Var (qualifiedName -> Just "GHC.Types.[]")) 
                  (Type (qualifiedName -> Just "GHC.Types.Char")))
  = Lit (MachStr "")
normalizeStringLiterals e = e

replaceDefinition :: String -> MaybeT MacroGen String
replaceDefinition "HaskellToMacro.Tokens.paren"    = lift $ getApplyUsage >>= \i -> return $ toThunk (mParenName ++ "_" ++ show i) 1
replaceDefinition "HaskellToMacro.Tokens.quote"    = return $ toThunk mQuoteName 1
replaceDefinition "GHC.Prim.+#"                    = return $ toThunk mAddName 2
replaceDefinition "GHC.Prim.-#"                    = return $ toThunk mSubName 2
replaceDefinition "GHC.Prim.*#"                    = return $ toThunk mMulName 2
replaceDefinition "GHC.Prim.quotInt#"              = return $ toThunk mDivName 2
replaceDefinition "GHC.Prim.remInt#"               = return $ toThunk mRemName 2
replaceDefinition "GHC.Prim.==#"                   = return $ toThunk mEqualsName 2
replaceDefinition "HaskellToMacro.Tokens.#"        = return $ toThunk mConcatName 2
replaceDefinition "HaskellToMacro.Tokens.##"       = return $ toThunk mConcatTokenName 2
replaceDefinition _                                = mzero


-- | Functions that does not change the value in the 
-- context of preprocessor macros
ignoredApp :: [String]
ignoredApp = ["unpackCString#"]
