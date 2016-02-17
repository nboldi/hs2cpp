{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module HighLevelFun (foldl,test) where

import HaskellToMacro.Prelude
import Data.Maybe (Maybe(Just))
import GHC.Base

{-# ANN foldl (GenerateRecursiveMacro (Just 10)) #-}
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

{-# ANN foldl GenerateMacro #-}
test :: Int
test = foldl (\(I# i) (I# j) -> I# (i +# j)) 0 [1,2,3,4]
