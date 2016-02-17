module HaskellToMacro.Plugin (plugin, pass) where

import GhcPlugins
import StaticFlags

import Data.IORef
import Control.Monad

import qualified HaskellToMacro.LetToLambda as LetToLambda
import HaskellToMacro.Macro
import HaskellToMacro.MacroGen
import HaskellToMacro.ConvertModule

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo =
  return (CoreDoPluginPass "LetToLambda" LetToLambda.pass 
           : CoreDoPluginPass "ToMacroPass" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass mod = do 
  reinitializeGlobals
  liftIO $ do ready <- readIORef v_opt_C_ready
              when (not ready) $ void $ parseStaticFlags []
  let outFileName = macroFileName $ moduleName (mg_module mod)
  ((),directives) <- runMacroGen $ convertModule mod
  liftIO $ writeFile outFileName $ directivesToStr directives
  return mod
