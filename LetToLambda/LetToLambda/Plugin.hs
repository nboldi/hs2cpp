module LetToLambda.Plugin (plugin, pass) where

import GhcPlugins

import LetToLambda.LetToLambda

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = return (CoreDoPluginPass "LetToLambda" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass mod = do
  reinitializeGlobals
  bindsOnlyPass (return . allLetsToToplevel) mod
