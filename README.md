# hs2cpp
Define preprocessor macro libraries in Haskell.

## Try out
1. Install HaskellToMacro as a ghc plugin. (cabal install)
2. Go to the Test directory
3. compile a testfile using the plugin: `ghc -fplugin HaskellToMacro.Plugin`

## Testing: 
with Test/Tester.hs:
 - `test`: test with all testfiles
 - `runTest "ModuleName"`: test ModuleName.hs
 - `preprocess "ModuleName"`: run preprocessor on ModuleName.c
 - `showCore "ModuleName"`: show core representation of ModuleName.hs
