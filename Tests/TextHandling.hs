{-# LANGUAGE OverloadedStrings #-}
module TextHandling (helloWorld, quotes, parens, comma, cat, emptyStr, oneCharStr, multiCharStr) where

import HaskellToMacro.Prelude

helloWorld :: TokenList
helloWorld = "Hello" # "==" # "World"

quotes :: TokenList
quotes = quote ("Hello" # "Quote")

parens :: TokenList
parens = paren ("Hello" # "Parens")

comma :: TokenList
comma = "Hello" # "," # "Comma"

cat :: TokenList -> TokenList -> TokenList
cat a b = a ## b

emptyStr :: TokenList
emptyStr = ""

oneCharStr :: TokenList
oneCharStr = "a"

multiCharStr :: TokenList
multiCharStr = "abcde"

