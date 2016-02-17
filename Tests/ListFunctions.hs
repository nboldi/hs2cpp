{-# LANGUAGE NoImplicitPrelude #-}
module ListFunctions ( map, (++), filter, concat, concatMap, head, last, tail, init, null, length, (!!), foldl, foldl1, scanl, scanl1, foldr, foldr1{-, scanr, scanr1-}, iterate, {-repeat, replicate, cycle,-} take, drop, splitAt, takeWhile, dropWhile, {-span, break, lines, words,-} unlines, unwords, reverse, and, or, any, all, elem, notElem, lookup, sum, product, maximum, minimum, {-zip, zip3, zipWith, zipWith3,-} unzip, unzip3 ) where

import Prelude hiding (map, (++), filter, concat, concatMap, head, last, tail, init, null, length, (!!), foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1, iterate, repeat, replicate, cycle, take, drop, splitAt, takeWhile, dropWhile, span, break, lines, words, unlines, unwords, reverse, and, or, any, all, elem, notElem, lookup, sum, product, maximum, minimum, zip, zip3, zipWith, zipWith3, unzip, unzip3)
import qualified Data.Char as Char

infixl 9 !!
infixr 5 ++
infix 4 `elem`, `notElem`

-- Map and append
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x = x : filter p xs
  | otherwise = filter p xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head :: [a] -> a
head (x:_) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_:xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs
init [] = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null (_:_) = False

length :: [a] -> Int
length [] = 0
length (_:l) = 1 + length l

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ [] = error "Prelude.foldl1: empty list"

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs
  = q : (case xs of
			[] -> []
			x:xs -> scanl f (f q x) xs)
			
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs) = scanl f x xs
scanl1 _ [] = []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 _ [] = error "Prelude.foldr1: empty list"

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
  
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []
  
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p xs@(x:xs')
  | p x = dropWhile p xs'
  | otherwise = xs
  
unlines :: [String] -> String
unlines = concatMap (++ "\n")

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ':s) ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

elem :: (Eq a) => a -> [a] -> Bool
elem x = any (== x)

notElem :: (Eq a) => a -> [a] -> Bool
notElem x = all (/= x)

lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x,y):xys)
  | key == x = Just y
  | otherwise = lookup key xys

sum :: (Num a) => [a] -> a
sum = foldl (+) 0

product :: (Num a) => [a] -> a
product = foldl (*) 1

maximum :: (Ord a) => [a] -> a
maximum [] = error "Prelude.maximum: empty list"
maximum xs = foldl1 max xs

minimum :: (Ord a) => [a] -> a
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

unzip :: [(a,b)] -> ([a],[b])
unzip = foldr (\(a,b) (ass,bs) -> (a:ass,b:bs)) ([],[])

unzip3 :: [(a,b,c)] -> ([a],[b],[c])
unzip3 = foldr (\(a,b,c) (ass,bs,cs) -> (a:ass,b:bs,c:cs)) ([],[],[])