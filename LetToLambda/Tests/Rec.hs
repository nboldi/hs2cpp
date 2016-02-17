module Rec(f) where

-- h is still polymorphic
f :: Int -> Int
f x = let h y = h x in h 4
