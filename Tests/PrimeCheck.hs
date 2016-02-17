{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, FlexibleInstances, MagicHash, ImplicitPrelude #-}
module PrimeCheck where

import Prelude hiding ((+), (-), (*), (/), (==), filter, rem, quot)
import GHC.TypeLits
import GHC.Base hiding ((==), (+), (-), (*), (/))
import HaskellToMacro.Prelude

showBool :: Bool -> TokenList
showBool True = "true"
showBool False = "false"

showInt :: Int -> TokenList
showInt (I# 0#) = "0"
showInt (I# 1#) = "1"
showInt (I# 2#) = "2"
showInt (I# 3#) = "3"
showInt (I# 4#) = "4"
showInt _       = "?"

(+) :: Int -> Int -> Int
(I# a) + (I# b) = I# (a +# b)

(-) :: Int -> Int -> Int
(I# a) - (I# b) = I# (a -# b)

(*) :: Int -> Int -> Int
(I# a) * (I# b) = I# (a *# b)

quot :: Int -> Int -> Int
quot (I# a) (I# b) = I# (quotInt# a b)

rem :: Int -> Int -> Int
rem (I# a) (I# b) = I# (remInt# a b)

empty :: [a] -> Bool
empty [] = True
empty _ = False

(==) :: Int -> Int -> Bool
(I# a) == (I# b) = case a ==# b of 0# -> False
                                   _  -> True

fromTo :: Recursive 25 => Int -> Int -> [Int]
fromTo from to 
  = if from == to 
      then [from]
      else from : fromTo (from+1) to
			
fromTo' :: Recursive 25 => Int -> Int -> [Int]
fromTo' from to 
  = if from == to 
      then [from]
      else [to]
	  
filter :: Recursive 25 => (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs 
                         else filter p xs
				 
divides :: Int -> Int -> Bool
divides b a 
  = a `rem` b == 0
				 
isPrime :: Int -> Bool
isPrime n = empty (filter (`divides` n) (fromTo 2 (quot n 2)))
						 
test_ x = fromTo 1 2 -- filter isPrime (fromTo 2 19)

test = test_ ()
