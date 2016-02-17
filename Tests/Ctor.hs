{-# LANGUAGE MagicHash #-}
module Ctor (Point(..), origo, point, xCoord, yCoord) where

import GHC.Base
import HaskellToMacro.Prelude

data Point = Point Int# Int#
  
origo = Point 0# 0# 

point = Point 3# 1#
  
xCoord (Point x y) = x  

yCoord (Point x y) = y
