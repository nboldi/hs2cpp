module Importer where

import HaskellToMacro.Prelude
import Imported

{-# ANN redefine GenerateMacro #-}
{-# NOINLINE redefine #-}  
redefine = importedDef

main = print redefine
