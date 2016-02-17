{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, FlexibleInstances #-}
module AnnotationClass (test, add, nToToken) where

import GHC.TypeLits
import HaskellToMacro.Prelude

data N = Z | S N

nToToken :: Recursive 100 => N -> TokenList
nToToken Z = "z"
nToToken (S i) = "s" # nToToken i

add :: Recursive 100 => N -> N -> N
add Z n = n
add (S n) m = add n (S m)

test = nToToken (add (S (S Z)) (S Z))
