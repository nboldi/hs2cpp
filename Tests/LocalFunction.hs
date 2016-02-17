{-# LANGUAGE MagicHash, OverloadedStrings #-}
module LocalFunction (test, nToToken) where

import GHC.Base
import HaskellToMacro.Prelude

data N = Z | S N

nToToken :: N -> TokenList
nToToken Z = "z"
nToToken (S _) = "s ?" 

{-# ANN test GenerateMacro #-}
test :: TokenList
test = let const :: N -> N -> N
           const x y = x 
        in nToToken (const (const Z (S Z)) (S (S Z)))
