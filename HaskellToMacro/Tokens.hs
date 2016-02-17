{-# LANGUAGE OverloadedStrings #-}
module HaskellToMacro.Tokens
  ( TokenList, Token, Tokens (..), (##), (#), paren, quote)
  where

import Data.String


-- | A restricted string that can be used to create efficient
-- storage of a string as simple text in macros.
data TokenList = TokenList String
type Token = TokenList

instance IsString TokenList where
  fromString = TokenList


-- | Make token sequence
(#) :: TokenList -> TokenList -> TokenList
TokenList s1 # TokenList s2 = TokenList (s1 ++ " " ++ s2)

-- | Token concatenation
(##) :: TokenList -> TokenList -> TokenList
TokenList s1 ## TokenList s2 = TokenList (s1 ++ s2)

infixl 5 #
infixl 6 ##


class Tokens a where
  tokenize :: a -> TokenList

instance Tokens Int where
  tokenize = fromString . show

-- TODO: use token pasting here instead of concat?
paren :: TokenList -> TokenList
paren mt = "(" # mt # ")"

-- TODO: use token pasting here instead of concat?
quote :: TokenList -> TokenList
quote mt = "\"" # mt # "\""
