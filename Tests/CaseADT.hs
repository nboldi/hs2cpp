{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts #-}
module CaseADT (mark, incomplete, N(..), isEven) where

import Prelude hiding (even)
import HaskellToMacro.Prelude

data Mark = A | B | C | D | E

mark :: Mark -> TokenList
mark A = "a"
mark B = "b"
mark C = "c"
mark D = "d"
mark E = "e"

incomplete :: Mark -> TokenList
incomplete A = "a"
incomplete B = "b"

data N = S N | Z

-- TODO: fix when top-level Recursive bindings are supported
isEven :: N -> TokenList
isEven = isEven'

isEven' :: Recursive 10 => N -> TokenList
isEven' Z = "even"
isEven' (S Z) = "odd"
isEven' (S (S n)) = isEven' n



