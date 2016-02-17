{-# LANGUAGE MagicHash #-}
module SimpleLet where

import GHC.Base
import HaskellToMacro.Prelude

main = print $ test

{-# NOINLINE test #-}
{-# ANN test GenerateMacro #-}
test :: Int
test = let i = 4# in I# (i +# i)
