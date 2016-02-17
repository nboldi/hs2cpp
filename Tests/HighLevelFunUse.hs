{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, NoImplicitPrelude, MagicHash, BangPatterns #-}
module HighLevelFunUse (fib,test) where

import HaskellToMacro.Prelude
import Data.Maybe (Maybe(Just))
import GHC.Base

-- sum :: Recursive 100 => [Int] -> Int
-- sum [] = 0
-- sum (I# x : xs) = let !(I# s) = sum xs in I# (x +# s)

-- to_num :: Recursive 100 => Int -> [Int]
-- to_num 0 = []
-- to_num (I# i) = I# i : to_num (I# (i -# 1#))

fib :: Recursive 10 => Int -> Int
fib 0 = 1
fib 1 = 1
fib (I# n) = let !(I# fib1) = fib (I# (n -# 1#)) 
                 !(I# fib2) = fib (I# (n -# 1#))
		      in I# (fib1 +# fib2)

test = fib 4
