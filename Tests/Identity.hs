module Identity (identity,test) where

import HaskellToMacro.Prelude

main = print $ test

{-# ANN identity GenerateMacro #-}
identity :: Int -> Int
identity i = i

{-# ANN test GenerateMacro #-}
test = identity 3