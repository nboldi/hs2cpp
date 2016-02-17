module HaskellToMacro.StdLib where

import Control.Monad

import HaskellToMacro.Macro
import HaskellToMacro.MacroGen


-- | Standard library macros
stdLib :: MacroGen ()
stdLib = do
  emitMacro (Comment "Standard library")
  -- tuple construction
  emitMacro $ macro "tuple" ["..."] "BOOST_PP_SEQ_FOLD_LEFT(tuple_op,BOOST_PP_CAT(tuple_,BOOST_PP_VARIADIC_SIZE(__VA_ARGS__)),BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__))"
  emitMacro $ macro "tuple_op" ["_", "thunk", "arg"] "HS2CPP_APPLY_0(thunk, arg)"
  forM [2 .. 64] $ \i -> do
    emitMacro $ macro ("tuple_" ++ show i) [] $ toMacroDefName $ tupleCtor i

  -- list construction
  emitMacro $ macro "nil" [] $ toMacroDefName "[]"
  emitMacro $ macro "cons" ["head", "tail"] $ "HS2CPP_APPLY_0(HS2CPP_APPLY_0(" ++ (toMacroDefName ":") ++ ", head),tail)"
  emitMacro $ macro "list" ["..."] "BOOST_PP_SEQ_FOLD_RIGHT(list_op,nil,BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__))"
  emitMacro $ macro "list_op" ["_", "tail", "head"] "cons(head,tail)"

-- | Generates a tuple constructor name
tupleCtor :: Int -> String
tupleCtor i = "(" ++ replicate (i - 1) ','  ++ ")"
