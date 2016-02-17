{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts #-}
module Recursion (test, add, nToToken) where

import HaskellToMacro.Prelude

data N = Z | S N

nToToken :: Recursive 10 => N -> TokenList
nToToken Z = "z"
nToToken (S i) = "s" # nToToken i
  
add :: Recursive 10 => N -> N -> N
add Z n = n
add (S n) m = add n (S m)

test = nToToken (add (S (S Z)) (S Z))
