{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, MagicHash #-}
module Config (Config(..),dump,itemConfig,hasPtrItem,arrayRank,declare,valueOf) where

import GHC.Base (Int(I#),Int#,(+#))
import Prelude hiding ((.),($),(+),id)
import HaskellToMacro.Prelude

-- Prelude redefinitions
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) g f x = g (f x)

(+) :: Int -> Int -> Int
(I# a) + (I# b) = I# (a +# b)

($) :: (a -> b) -> a -> b
f $ x =  f x

infixr 0 $

id :: a -> a
id x = x


-- Data types
type Code = TokenList
type TypeName = Token
type VarName = Token

data Config = Scalar | Pointer Config | Array Int Config

-- Pretty printing
dump :: Config -> Code
dump = quote . dumpStr
  where
    dumpStr :: Recursive 10 => Config -> Token
    dumpStr Scalar      = "S"
    dumpStr (Pointer c) = "P" # dumpStr c
    dumpStr (Array d c) = "A" # tokenize d # dumpStr c

-- Item configuration
itemConfig :: Config -> Config
itemConfig Scalar      = Scalar
itemConfig (Pointer c) = itemConfig c
itemConfig (Array d c) = c

-- Item configuration pointer checking
hasPtrItem :: Config -> Bool
hasPtrItem = hasPtrItem' . itemConfig
  where
    hasPtrItem' :: Recursive 10 => Config -> Bool
    hasPtrItem' Scalar      = False
    hasPtrItem' (Pointer c) = True
    hasPtrItem' (Array d c) = hasPtrItem' c

-- Parenthesizes code when the configuration is an array
-- parenArray :: Config -> Code -> Code
-- parenArray c = case c of
  -- (Array _ _) -> paren 
  -- _ -> id
  
-- Value of a scalar
valueOf :: Code -> Config -> Code
valueOf code conf = valueOf' conf code
  where
    valueOf' :: Recursive 10 => Config -> Code -> Code
    valueOf' Scalar c = c
    valueOf' (Pointer conf) c = "*" # valueOf' conf c
    valueOf' (Array _ _) c = "do not use valueOf on Arrays"

-- Array rank (the number of dimensions) of a configuration
arrayRank :: Config -> Int
arrayRank = arrayRank'
  where
    arrayRank' :: Recursive 10 => Config -> Int
    arrayRank' Scalar      = 0
    arrayRank' (Pointer c) = arrayRank c
    arrayRank' (Array d c) = arrayRank c + 1

-- Declare variable
declare :: TypeName -> VarName -> Config -> Code
declare baseType var c = baseType # typedName c # ";"
  where   
    typedName :: Recursive 10 => Config -> Code
    typedName Scalar      = var
    typedName (Pointer (Array d c)) = paren ("*" # typedName c) # "[" # tokenize d # "]"
    typedName (Pointer c) = "*" # typedName c
    typedName (Array d c) = typedName c # "[" # tokenize d # "]"
    
