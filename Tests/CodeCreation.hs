{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DataKinds, FlexibleContexts #-}
module CodeCreation (mkStruct, mkField, derive, test) where

import HaskellToMacro.Prelude
import Data.Maybe (Maybe(Just))
-- import HighLevelFun(foldl)

foldl :: Recursive 10 => (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

map :: Recursive 10 => (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

concat :: [TokenList] -> TokenList
concat = doConcat ""
  where doConcat c [] = c
        doConcat c (x : xs) = doConcat (c # x) xs

-- Struct construction
type Code = TokenList
type StructName = TokenList

mkStruct :: StructName -> [Field] -> Deriving -> Code
mkStruct name fields (Deriving classNames) = mkStructDef # mkFunctions
  where
    mkStructDef :: Code
    mkStructDef = "typedef struct {"
                 # mkFieldDefs
                 # "}" # name # ";"

    mkFieldDefs :: Code
    mkFieldDefs = foldl singleFieldDef "" fields

    singleFieldDef :: Code -> Field -> Code
    singleFieldDef code (Field fieldType fieldName) 
      = code # getTypeName fieldType # fieldName # ";"

    mkFunctions :: Code
    mkFunctions = concat (map singleFunction classNames)

    singleFunction :: ClassName -> Code
    singleFunction ClassEq = mkEqFunction
    singleFunction ClassOrd = mkOrdFunction
    singleFunction ClassShow = mkShowFunction

    mkEqFunction :: Code
    mkEqFunction = "bool"
              # (name ## "_equals") # paren (name # "*a," # name # "*b") # "{"
              # "return " # (foldl mkEqExpr "true" fields) # ";" # "}"

    mkEqExpr :: Code -> Field -> Code
    mkEqExpr code (Field _ fieldName) 
      = code # "&&" # "a->" # fieldName # "==" # "b->" # fieldName # ""

    mkOrdFunction :: Code
    mkOrdFunction = "ordering "
        # name ## "_compare" # paren (name # " *a, " # name # " *b") # "{"
        # (foldl mkOrdStmts "" fields)
        # "return eq;}"

    mkOrdStmts :: Code -> Field -> Code
    mkOrdStmts code (Field _ fieldName) = code
        # "if" # paren ("a->" # fieldName # " < b->" # fieldName) # "return lt;"
        # "if" # paren ("a->" # fieldName # " > b->" # fieldName) # "return gt;"

    mkShowFunction :: Code
    mkShowFunction = "void "
        # name ## "_show" # paren (name # " *a") # "{"
        # (foldl mkShowStmt "" fields)
        # "}"

    mkShowStmt :: Code -> Field -> Code
    mkShowStmt code (Field fieldType fieldName) = code
        # "printf" # paren (quote (fieldName # ":" # getFormatString fieldType)
        # ", a->" # fieldName) # ";"

    getFormatString :: FieldType -> TokenList
    getFormatString FTInt = "%d"

    getTypeName :: FieldType -> TokenList
    getTypeName FTInt = "int"

-- Field type and construction
data FieldType = FTInt
type FieldName = TokenList
data Field = Field FieldType FieldName

mkField :: FieldType -> FieldName -> Field
mkField = Field


-- Type class handling
data ClassName = ClassEq | ClassOrd | ClassShow
data Deriving = Deriving [ClassName]

derive :: [ClassName] -> Deriving
derive = Deriving

-- Test
test :: TokenList
test = mkStruct "point"
             [ mkField FTInt "x"
             , mkField FTInt "y"
             ] (derive [ClassEq, ClassOrd, ClassShow ])
