{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module CaseApply (test) where

import HaskellToMacro.Prelude

data Jegy = Egyes | Kettes | Harmas | Negyes | Otos
  deriving Show

{-# ANN test GenerateMacro #-}
test :: TokenList
test = (\case Egyes -> "bukas"; _ -> "atment") Harmas

