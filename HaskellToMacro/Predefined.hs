module HaskellToMacro.Predefined where

import GhcPlugins
import PrelNames

import Data.List
import Control.Applicative
import Control.Monad

import HaskellToMacro.Macro
import HaskellToMacro.MacroGen
import HaskellToMacro.StdLib

-- | Generation of predefined macros
defaultMacros :: Int -> MacroGen ()
defaultMacros applyRepeat = do
  forM [0 .. applyRepeat - 1] $ \i -> do
    -- function (thunk) application
    let args = mSeqPushBack (mThunkArgs "f") (return "a")
        newThunk = mToThunk (mThunkFun "f") (mThunkArgN "f") args
        evaluated i = mExpand i ( mApplyWhen (mEquals (mThunkArgN "f") (mSeqSize args)) (mThunkFun "f") (mSeqToTuple args) )
    emitMacro . macro (mApplyName ++ "_" ++ show i) ["f","a"]
      =<< mIf' i (mEquals (mSeqSize args) (mThunkArgN "f")) (evaluated i) newThunk
    -- expansion function
    emitMacro (macro (mExpandName ++ "_" ++ show i) ["t"] "t")
    -- check if a the pattern match exception is an exception
    emitMacro . macro (mTryName ++ "_" ++ show i) ["f","t"]
      =<< mIf' i (mIsException (return "t")) (return "t") (mApp "f" [return "t"])
    emitMacro . macro (mTryCtxName ++ "_" ++ show i) ["f", "t", "r..."]
      =<< mIf' i (mIsException (return "t")) (return "t") (mApp "f" [return "r", return "t"])
    -- generates if macros
    emitMacro . macro (mIfName ++ "_" ++ show i) ["p", "t", "f"]
      =<< mConcat (return $ mIfName ++ "_" ++ show i ++ "_")
                  (return "p")
            <++> return "(t,f)"
    emitMacro $ macro (mIfName ++ "_" ++ show i ++ "_1") ["t", "f"] "t"
    emitMacro $ macro (mIfName ++ "_" ++ show i ++ "_0") ["t", "f"] "f"
    emitMacro $ macro (mExprIfName ++ "_" ++ show i) ["p", "t"] (mIfName ++ "_" ++ show i ++ "(p,t,)")
    emitMacro $ macro (mExprIfName ++ "_" ++ show i) ["p", "t"] (mIfName ++ "_" ++ show i ++ "(p,t,)")
    
    emitMacro . macro (mParenName ++ "_" ++ show i) ["txt"]
      =<< mToVal (parenM (mGetVal "txt"))

  emitMacro . macro mApplyWhenName ["p", "f", "args"]
    =<< return "f " <++> (getApplyUsage >>= \i -> mExprIf' i (mNot (return "p")) (return "$")) <++> return "args"
  emitMacro . macro mToMacroStrName ["str"]
    =<< mApp (mToMacroStrName ++ "_0") [return "str"]
  emitMacro . macro mListMemberName ["e", "l"]
    =<< mListIsCons (mListFilter (return mListMemberPredName) (return "e") (return "l"))
  emitMacro . macro mListMemberPredName ["d", "data", "elem"]
    =<< mEquals (return "data") (return "elem")
  -- TODO: use try
  emitMacro . macro mAddName ["a","b"]
    =<< mToVal (mAdd (mGetVal "a") (mGetVal "b"))
  emitMacro . macro mSubName ["a","b"]
    =<< mToVal (mSub (mGetVal "a") (mGetVal "b"))
  emitMacro . macro mMulName ["a","b"]
    =<< mToVal (mMul (mGetVal "a") (mGetVal "b"))
  emitMacro . macro mDivName ["a","b"]
    =<< mToVal (mDiv (mGetVal "a") (mGetVal "b"))
  emitMacro . macro mRemName ["a","b"]
    =<< mToVal (mRem (mGetVal "a") (mGetVal "b"))
  emitMacro . macro mEqualsName ["a","b"]
    =<< mToVal (mEquals (mGetVal "a") (mGetVal "b"))
  emitMacro . macro mConcatName ["txt1","txt2"]
    =<< mToVal (parenM (mRemoveParen (mGetVal "txt1") <++> mRemoveParen (mGetVal "txt2")))
  emitMacro . macro mConcatTokenName ["txt1","txt2"]
    =<< mToVal (parenM (mConcat (mRemoveParen (mGetVal "txt1")) (mRemoveParen (mGetVal "txt2"))))
  emitMacro . macro mIsExceptionName ["o"] 
    =<< (mEquals (mSeqElem 0 (return "o")) (return mExceptionName))
  emitMacro . macro mUnwrapName ["o"] 
    =<< mSeqElem 1 (return "o")
  emitMacro $ macro mValueName [] "0"
  emitMacro $ macro mThunkName [] "1"
  emitMacro $ macro mExceptionName [] "2"
  emitMacro . macro mRemoveParenName ["x..."]
    =<< mEval (return $ mRemoveParenIIName ++ " x")
  emitMacro $ macro mEvalName ["x..."] "x"
  emitMacro . macro mStringizeName ["x..."]
    =<< mApp mStringizeIName [return "x"]
  emitMacro $ macro mStringizeIName ["x..."] "#x"
  emitMacro $ macro mRemoveParenIIName ["x..."] "x"
  emitMacro . macro mQuoteName ["txt"]
    =<< mToVal (parenM (mStringize (mRemoveParen (mGetVal "txt"))))
  stdLib

-- TODO: only add includes that are needed
boostIncludes
  = map (Include . (++".hpp") . ("boost/preprocessor/"++))
      [ "control/if", "control/iif"
      , "arithmetic/sub", "arithmetic/add", "arithmetic/mul", "arithmetic/div", "arithmetic/mod"
      , "list/adt", "list/size", "list/at", "list/filter", "list/to_tuple", "list/append"
      , "seq/size", "seq/push_back", "seq/to_tuple", "seq/seq", "seq/fold_left", "seq/fold_right"
      , "tuple/rem", "tuple/elem"
      , "variadic/size", "variadic/to_seq"
      , "logical/not"
      , "cat", "comparison/equal", "stringize" ]

-- * Names of predefined macros

mApplyWhenName = "HS2CPP_APP_WHEN"
mValueName = "HS2CPP_VALUE"
mThunkName = "HS2CPP_THUNK"
mExceptionName = "HS2CPP_EXCEPTION"
mApplyName = "HS2CPP_APPLY"
mTryName = "HS2CPP_TRY"
mTryCtxName = "HS2CPP_TRY_CTX"
mListMemberName = "HS2CPP_LIST_MEMBER"
mListMemberPredName = "HS2CPP_LIST_MEMBER_PRED"
mAddName = "HS2CPP_ADD"
mSubName = "HS2CPP_SUB"
mMulName = "HS2CPP_MUL"
mDivName = "HS2CPP_DIV"
mRemName = "HS2CPP_REM"
mEqualsName = "HS2CPP_EQUALS"
mExpandName = "HS2CPP_EXPAND"
mIfName = "HS2CPP_IF"
mExprIfName = "HS2CPP_EXPR_IF"
mToMacroStrName = "HS2CPP_TO_MACRO_STR"
mConcatName = "HS2CPP_CONCAT"
mConcatTokenName = "HS2CPP_CONCAT_TOKEN"
mParenName = "HS2CPP_PAREN"
mQuoteName = "HS2CPP_QUOTE"
mEvalName = "HS2CPP_EVAL"
mRemoveParenName = "HS2CPP_REMOVE_PAREN"
mRemoveParenIIName = "HS2CPP_REMOVE_PAREN_II"
mStringizeName = "HS2CPP_STRINGIZE"
mStringizeIName = "HS2CPP_STRINGIZE_I"
mIsExceptionName = "HS2CPP_IS_EXCEPTION"
mUnwrapName = "HS2CPP_UNWRAP"

-- * Shallow embedding of predefined macros

mApp :: String -> [MacroGen String] -> MacroGen String
mApp f args
  = do argVals <- sequence args
       return $ f ++ if (length argVals > 0)
                       then "(" ++ intercalate "," argVals ++ ")"
                       else ""
app :: String -> [String] -> String
app f args = f ++ if (length args > 0)
                       then "(" ++ intercalate "," args ++ ")"
                       else ""

infixl 5 <++>
-- | Lifted simple string concatenation
(<++>) :: MacroGen [a] -> MacroGen [a] -> MacroGen [a]
(<++>) = liftM2 (++)

mApply f a = getApplyUsage >>= \i -> mApp (mApplyName ++ "_" ++ show i) [f,a]
mApplyWhen p f args = mApp mApplyWhenName [p, f, args]
mTry t f = getApplyUsage >>= \i -> mApp (mTryName ++ "_" ++ show i) [t,f]
mTryCtx t f r = getApplyUsage >>= \i -> mApp (mTryCtxName ++ "_" ++ show i) ([t,f] ++ r)
mIf p t f = getApplyUsage >>= \i -> mApp (mIfName ++ "_" ++ show i) [p,t,f]
mIf' i p t f = mApp (mIfName ++ "_" ++ show i) [p,t,f]
mExprIf p t = getApplyUsage >>= \i -> mApp (mExprIfName ++ "_" ++ show i) [p,t]
mExprIf' :: Int -> MacroGen String -> MacroGen String -> MacroGen String
mExprIf' i p t = mApp (mExprIfName ++ "_" ++ show i) [p,t]
mRepeat n m d = mApp "BOOST_PP_REPEAT" [n,m,d]
mAdd x y = mApp "BOOST_PP_ADD" [x,y]
mSub x y = mApp "BOOST_PP_SUB" [x,y]
mMul x y = mApp "BOOST_PP_MUL" [x,y]
mDiv x y = mApp "BOOST_PP_DIV" [x,y]
mRem x y = mApp "BOOST_PP_MOD" [x,y]
mNot p = mApp "BOOST_PP_NOT" [p]
mExpand i a = mApp (mExpandName ++ "_" ++ show (i :: Int)) [a]

mSeqNil = ""
mSeqSize s = mApp "BOOST_PP_SEQ_SIZE" [s]
mSeqPushBack s e = mApp "BOOST_PP_SEQ_PUSH_BACK" [s, e]
mSeqAppend s1 s2 = s1 <++> s2
mSeqToTuple s = mApp "BOOST_PP_SEQ_TO_TUPLE" [s]
mSeqElem i s = mApp "BOOST_PP_SEQ_ELEM" [return (show i), s]

mListMember e ls = mApp mListMemberName [e, ls]
mNilList = "BOOST_PP_NIL"
mCons e ls = mApp "BOOST_PP_LIST_CONS" [e, ls]
mTupAt s i t = mApp "BOOST_PP_TUPLE_ELEM" $ map return [show s, show i, t]
mTupAtM s i t = mApp "BOOST_PP_TUPLE_ELEM" [return (show s), return (show i), t]
mConcat a b = mApp "BOOST_PP_CAT" [a, b]
mEquals a b = mApp "BOOST_PP_EQUAL" [a, b]
mNotEqual a b = mApp "BOOST_PP_NOT_EQUAL" [a, b]
mTupToArgs s t = mApp "BOOST_PP_TUPLE_REM_CTOR" [s, t]
mListAt ls i = mApp "BOOST_PP_LIST_AT" [ls, return (show i)]
mListLength ls = mApp "BOOST_PP_LIST_SIZE" [ls]
mListAppend a b = mApp "BOOST_PP_LIST_APPEND" [a, b]
mListIsCons ls = mApp "BOOST_PP_LIST_IS_CONS" [ls]
mListToTuple ls = mApp "BOOST_PP_LIST_TO_TUPLE" [ls]
mListFilter pred d ls = mApp "BOOST_PP_LIST_FILTER" [pred, d, ls]
mRevertList ls = mApp "BOOST_PP_LIST_REVERSE" [ls]
mToMacroStr ls = mApp mToMacroStrName [ls]
mStringize txt = mApp mStringizeName [txt]
mParen txt = getApplyUsage >>= \i -> mApp (mParenName ++ "_" ++ show (i :: Int)) [txt]
mRemoveParen txt = mApp mRemoveParenName [txt]
mEval txt = mApp mEvalName [txt]

-- ** Abstraction of value accessors
mUpdateVal u v = mToVal
mGetVal = mSeqElem 1 . return

mToVal v = toSeqM [ return mValueName, v ]
mIsVal o = mEquals (mSeqElem 0 (return o)) (return mValueName)

mGetADTTag = mTupAtM 2 0
mGetADTArgs = mTupAtM 2 1

mThunkFun = mSeqElem 1 . return
mThunkArgN = mSeqElem 2 . return
mThunkArgs = mSeqElem 3 . return
mToThunk f n args = toSeqM [ return mThunkName, f, n, args ]
mIsThunk o = mEquals (mSeqElem 0 (return o)) (return mThunkName)

mIsException o = mApp mIsExceptionName [o]

toException :: String -> String
toException msg = toSeq [mExceptionName, msg]

-- * Creation of data types

toValue :: String -> String
toValue v = toSeq [mValueName, v]

toThunk :: String -> Int -> String
toThunk f n = toSeq [mThunkName, f, show n, mSeqNil]

toPartlyAppliedThunk :: String -> Int -> [String] -> String
toPartlyAppliedThunk f n args = toSeq [mThunkName, f, show (n + length args), toSeq args]

toTuple :: [String] -> String
toTuple l = "(" ++ concat (intersperse "," l) ++ ")"

toTupleM :: [MacroGen String] -> MacroGen String
toTupleM ls = toTuple <$> sequence ls

toArray :: [String] -> String
toArray l = toTuple [show (length l), toTuple l]

toSeq :: [String] -> String
toSeq [] = mSeqNil
toSeq l = concat $ map (\e -> "(" ++ e ++ ")") l

toSeqM :: [MacroGen String] -> MacroGen String
toSeqM ls = toSeq <$> sequence ls

toList :: [String] -> String
toList (e:l) = toTuple [e, toList l]
toList [] = mNilList

parenM :: MacroGen String -> MacroGen String
parenM mg = return "(" <++> mg <++> return ")"
