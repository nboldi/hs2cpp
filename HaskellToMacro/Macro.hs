module HaskellToMacro.Macro where

import Data.List
import Data.Char

-- | A low-level encoding of macro definitions and comments
data Directive 
  = Macro { macroName :: String
          , macroArgs :: Maybe [String]
          , macroBody :: String
          }
  | Include { includePath :: String }
  | Comment { commentTxt :: String }

-- | Creates a macro with a correct name
macro :: String -> [String] -> String -> Directive
macro name [] = Macro (toMacroDefName name) Nothing
macro name args = Macro (toMacroDefName name) (Just args)

macroWithParen :: String -> String -> Directive
macroWithParen name = Macro (toMacroDefName name) (Just [])
              
-- | The source code for the macro definition            
outputDirective :: Directive -> String
outputDirective (Macro name Nothing body)
  = "#define " ++ name ++ " " ++ body
outputDirective (Macro name (Just args) body)
  = "#define " ++ name ++ "(" ++ intercalate "," args ++ ") " ++ body 
outputDirective (Include path)
  = "#include " ++ show path
outputDirective (Comment txt)
  = concat $ intersperse "\n" $ map ("// " ++) (lines txt)

directivesToStr :: [Directive] -> String
directivesToStr = concat . map ((++"\n") . outputDirective)
  
-- | Escapes characters in a given name, that cannot be part of a macro name.
toMacroDefName :: String -> String
toMacroDefName = concatMap (\c -> if toEscape c then "_" ++ show (ord c) else [c])
  where toEscape c = not $ isAlphaNum c || (c == '_')
 
