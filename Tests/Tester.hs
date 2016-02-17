module Tester where

import Test.HUnit hiding (test)
import System.Exit
import System.Process
import System.IO
import Data.Char

test = runTestTT tests

tests = TestList (map createTest testFixtures)

createTest (modul, append, result) 
  = TestLabel (modul ++ ":" ++ append) 
     $ TestCase $ do (runGhc modul) >>= checkProcess "compilation"
                     withFile (modul ++ ".h") AppendMode (\h -> hPutStrLn h ("\n" ++ append))
                     pp@(_,out,_) <- runPreproc modul
                     checkProcess "preprocessing" pp
                     assertEqual ("The preprocessing did not give the expected result: " ++ out ++ "\n") 
                                 (removeWS result) (removeWS out)
   
runGhc modul = readProcessWithExitCode "ghc" (ghcArgs modul) ""
ghcArgs modul = ["-fforce-recomp", "-fplugin", "HaskellToMacro.Plugin", modul ++ ".hs"]
runPreproc modul = readProcessWithExitCode "gcc" ["-std=c99", "-E", "-P", "-I.", modul ++ ".h"] ""
checkProcess task (exit,out,err) 
  = assertBool ("The " ++ task ++ " did not finish successfully:\n" ++ err ++ out) (ExitSuccess == exit)
removeWS = filter (not . isSpace)
      
preprocess modul = do (_,out,err) <- runPreproc modul
                      putStrLn (err ++ out)
      
runTest modul 
  = runTestTT $ TestList
      $ map createTest (filter (\(m,_,_) -> m == modul) testFixtures)
        
showCore modul 
  = do (_,_,_,handle) <- createProcess (proc "ghc" ["-fforce-recomp", "-fplugin", "HaskellToMacro.PrettyPrint", modul ++ ".hs"])
       waitForProcess handle
  
testFixtures = 
  [ ("CaseADT"              , "mark(C)"             , "(0)(c)")
  , ("CaseADT"              , "incomplete(A)"       , "(0)(a)")
  , ("CaseADT"              , "isEven(Z)"           , "(0)(even)")
  , ("CaseADT"              , "isEven(S(Z))"        , "(0)(odd)")
  , ("CaseADT"              , "isEven(S(S(Z)))"     , "(0)(even)")
  , ("CasePrim"             , "mark(3)"             , "(0)(C)")
  , ("CasePrim"             , "mark(6)"             , "(0)(unknown mark)")
  , ("CaseApply"            , "test"                , "(0)(atment)")
  , ("Ctor"                 , "origo"               , "(0)((1,((0)(0))((0)(0))))")
  , ("Ctor"                 , "Point(4,6)"          , "(0)((1,((0)(4))((0)(6))))")
  , ("Ctor"                 , "xCoord(point)"       , "(0)(3)")
  , ("Ctor"                 , "yCoord(point)"       , "(0)(1)")
  , ("Ctor"                 , "xCoord(Point(4,6))"  , "(0)(4)")
  , ("Exceptions"           , "myError"             , "(2)(AAA)")
  , ("Identity"             , "test"                , "(0)(3)")
  , ("SimpleLet"            , "test"                , "(0)(8)")
  , ("TextHandling"         , "emptyStr"            , "(0)()")
  , ("TextHandling"         , "oneCharStr"          , "(0)(a)")
  , ("TextHandling"         , "multiCharStr"        , "(0)(abcde)")
  , ("TextHandling"         , "cat(a,b)"            , "(0)(ab)")
  , ("TextHandling"         , "helloWorld"          , "(0)(Hello==World)")
  , ("TextHandling"         , "parens"              , "(0)((HelloParens))")
  , ("TextHandling"         , "quotes"              , "(0)(\"Hello Quote\")")
  , ("TextHandling"         , "comma"               , "(0)(Hello,Comma)")
  , ("LocalFunction"        , "test"                , "(0)(z)")
  , ("RecursiveLet"         , "countdown"           , "(0)((1,))")
  , ("NameShadowing"        , "test"                , "(0)(6)")
  , ("Importer"             , "redefine"            , "(0)(42)")
  -- , ("Newtype"              , "unwrapped", "(0,Hello)")
  , ("Class"                , "test"                , "(0)(a)")
  , ("Tuple"                , "test"                , "(0)(1)")
  , ("BoostReplacedFunction", "add(1,2)"            , "(0)(3)")
  , ("AutoWrap"             , "add(1,2)"            , "(0)(3)")
  , ("AutoWrap"             , "addI(1,2)"           , "(0)(3)")
  , ("Recursion"            , "test"                , "(0)(sssz)")
  , ("AnnotationClass"      , "test"                , "(0)(sssz)")
  , ("MutualRecursion"      , "countdown"           , "(0)(z)")
  , ("HighLevelFun"         , "test"                , "(0)(10)")
  -- , ("HighLevelFunUse"      , "test"                , "(0)((_6c,((0)(10))))")
  , ("CodeCreation"         , "test"                , "(0)(" ++ codeCreated ++ ")")
  -- , ("PrimeCheck"           , "test"     , "")
  ]

codeCreated = "typedef struct { int x; int y; } point;"
  ++ "bool point_equals(point *a, point *b) {"
    ++ "return true && a->x == b->x && a->y == b->y;"
  ++ "}"
  ++ "ordering point_compare(point *a, point *b) {"
    ++ "if (a->x < b->x) return lt;"
    ++ "if (a->x > b->x) return gt;"
    ++ "if (a->y < b->y) return lt;"
    ++ "if (a->y > b->y) return gt;"
    ++ "return eq;"
  ++ "}"
  ++ "void point_show(point *a) {"
    ++ "printf(\"x: %d\",a->x);"
    ++ "printf(\"y: %d\",a->y);"
  ++ "}"
  
