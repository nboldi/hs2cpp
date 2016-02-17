{-# LANGUAGE MagicHash #-}
module BoostReplacedFunction (add) where

import HaskellToMacro.Prelude

import GHC.Base

add :: Int# -> Int# -> Int
add x y = I# (x +# y)
