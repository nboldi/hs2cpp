module RecursiveLet where

import HaskellToMacro.Prelude

main = print $ countdown

data N = Z | S N
  deriving Show

{-# NOINLINE countdown #-}
{-# ANN countdown GenerateMacro #-}
countdown :: N
countdown = let tick Z = Z 
                tick (S i) = tack i
                tack i = tick i 
             in tack (S (S Z))
