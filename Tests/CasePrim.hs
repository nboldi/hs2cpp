{-# LANGUAGE OverloadedStrings #-}
module CasePrim (mark) where

import HaskellToMacro.Prelude

mark :: Int -> TokenList
mark 1 = "E"
mark 2 = "D"
mark 3 = "C"
mark 4 = "B"
mark 5 = "A"
mark _ = "unknown mark"
