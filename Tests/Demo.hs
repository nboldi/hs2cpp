{-# LANGUAGE OverloadedStrings, MagicHash #-}
-- exported definitions can be used
module Demo (mark, sumSqr) where

import GHC.Base (Int(..), (+#), (*#))
import Prelude hiding (Num, (+), (*))
import HaskellToMacro.Prelude

-- Must redefine classes like Num in order to be able to use them. 
infixl 5 + 
infixl 6 *
class  Num a  where
    (+), (*) :: a -> a -> a
    -- ...

instance  Num Int  where
    I# x + I# y = I# (x +# y)
    I# x * I# y = I# (x *# y)
    -- ...

-- Class instances are resolved
sumSqr :: Int -> Int -> Int
sumSqr a b = a * a + b * b

-- The defined data types can be used
data Mark = A | B | C | D | E

mark :: Mark -> TokenList
mark A = "a"
mark B = "b"
mark C = "c"
-- explicit errors are handled
mark D = error "cannot give a d"
-- error is generated if E is given
