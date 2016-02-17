{-# LANGUAGE MagicHash #-}
module AutoWrap (add, addI) where

import HaskellToMacro.Prelude

import GHC.Base

add :: Int# -> Int# -> Int
add x y = I# (x +# y)

addI :: Int -> Int -> Int
addI (I# x) (I# y) = I# (x +# y)
