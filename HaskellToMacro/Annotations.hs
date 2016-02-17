{-# LANGUAGE DeriveDataTypeable, DataKinds, KindSignatures, FlexibleInstances #-}
module HaskellToMacro.Annotations
  ( MacroAnnotation (..), Recursive
  )  where

import Data.Data
import GHC.TypeLits


data MacroAnnotation = GenerateMacro | GenerateRecursiveMacro (Maybe Int)
  deriving (Typeable, Data)

class Recursive (i :: Nat)
instance Recursive i
