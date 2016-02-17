module NameShadowing where

import HaskellToMacro.Prelude

main = print test

{-# ANN ignore GenerateMacro #-}
{-# NOINLINE ignore #-}  
ignore :: Int -> Int -> Int
ignore a = \a -> a

{-# ANN test GenerateMacro #-}
{-# NOINLINE test #-}  
test = ignore 3 6