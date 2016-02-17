{-# LANGUAGE OverloadedStrings #-}
module Class (test) where

import HaskellToMacro.Prelude

class ToToken t where
  token :: t -> TokenList
  parenToken :: t -> TokenList
  
data A = A
instance ToToken A where 
  token A = "a"
  parenToken A = "(a)"

data B = B
instance ToToken B where 
  token B = "b"
  parenToken B = "(b)"
  
test :: TokenList
test = token A