module Tuple where

import HaskellToMacro.Prelude

main = print test

newtype WrapStr = WrapStr String

{-# ANN tup GenerateMacro #-}
{-# NOINLINE tup #-}  
tup :: (Int,Int)
tup = (1,2)

{-# ANN first GenerateMacro #-}
{-# NOINLINE first #-}  
first (a,b) = a

{-# ANN test GenerateMacro #-}
{-# NOINLINE test #-}
test = first tup