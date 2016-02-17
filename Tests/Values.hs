{-# LANGUAGE OverloadedStrings #-}
module Values (a, fourtyTwo, truth, thunk, IntList(..)) where

import HaskellToMacro.Prelude

a :: Char
a = 'a'

fourtyTwo :: Int
fourtyTwo = 42

truth :: Bool
truth = True

thunk :: Int -> Int
thunk = div 42

data IntList = Nil | Cons Int IntList
