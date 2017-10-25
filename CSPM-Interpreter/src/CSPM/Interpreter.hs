----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- An API for the Interpreter.
--
----------------------------------------------------------------------------

module CSPM.Interpreter
(
   runFile
  ,evalString
  ,evalFile
  ,getSigma
  ,prepareAST
  ,runInterpreter
  ,runInterpreterP
  ,module CSPM.Interpreter.Types
  ,module CSPM.Interpreter.Bindings
  ,interpreterVersion
)
where

import CSPM.Interpreter.Types
import CSPM.Interpreter.CoreInstances ()
import CSPM.Interpreter.Bindings
import CSPM.Interpreter.Eval
import CSPM.Interpreter.PrepareAST
import CSPM.Interpreter.Test.CLI
import CSPM.Interpreter.Pretty ()

import Language.CSPM.AST as AST

import Paths_CSPM_Interpreter (version)
import Data.Version (Version)
import Data.IntMap as IntMap

-- | The version of the CSPM-Interpreter library
interpreterVersion :: Version
interpreterVersion = version

-- | Run the interpreter for a given module and top-level identifier.
runInterpreter :: AST.Module INT -> AST.UniqueIdent -> (Value, Env)
runInterpreter ast entry
  = (getLetBindings env IntMap.! AST.uniqueIdentId entry, env)
  where 
    env = evalModule ast

-- | Run the interpreter for a given module and top-level identifier.
-- Cast result to a process
runInterpreterP :: AST.Module INT -> AST.UniqueIdent -> (Process, Env)
runInterpreterP ast entry
  = case val of
      VProcess x -> (x, env)
      _ -> throwTypingError
              "entrypoint is not a CSPM-process"
              (Just $ AST.bindingLoc entry)
              (Just val)
  where (val, env) = runInterpreter ast entry