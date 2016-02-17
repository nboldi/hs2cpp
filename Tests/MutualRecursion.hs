{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts #-}
module MutualRecursion (countdown, tick, tack, nToToken) where

import HaskellToMacro.Prelude

data N = Z | S N

nToToken :: Recursive 10 => N -> TokenList
nToToken Z = "z"
nToToken (S i) = "s" # nToToken i
  
countdown :: TokenList
countdown = nToToken (tack (S (S Z)))

tick :: Recursive 10 => N -> N
tick Z = Z 
tick (S i) = tack i

tack :: Recursive 10 => N -> N
tack i = tick i 
