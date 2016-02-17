module Newtype where

import HaskellToMacro.Prelude

main = print unwrapped

newtype WrapStr = WrapStr String

{-# ANN wrapped GenerateMacro #-}
{-# NOINLINE wrapped #-}  
wrapped :: WrapStr
wrapped = WrapStr "Hello"

{-# ANN unwrap GenerateMacro #-}
{-# NOINLINE unwrap #-}  
unwrap (WrapStr s) = s

{-# ANN unwrapped GenerateMacro #-}
{-# NOINLINE unwrapped #-}
unwrapped = unwrap wrapped