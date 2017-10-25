----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Test.CLI
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This is mainly useful for testing the functional sub language.
-- This module does not allow tracing of processes
-- (tracing is implemented in an other package).
--
-- 'runFile' loads a CSPM-specification from a file and evaluates an expression in
-- the context of that specification.
--
-- Example:
--
--    'runFile' fib.csp fib(10)
--
-- where the file fib.csp contains:
--    fib(x)= if x <2 then 1 else fib(x-1)+fib(x-2)
--
-- 'runFile' writes to 'stdout' and handles some exceptions.
--
----------------------------------------------------------------------------

module CSPM.Interpreter.Test.CLI
(
  runFile
  ,evalFile
  ,evalString
)

where
import Language.CSPM.Frontend
import qualified Language.CSPM.AST as AST

import qualified CSPM.Interpreter.Types as Types
import CSPM.Interpreter.Eval
import CSPM.Interpreter.Types (Value)
import CSPM.Interpreter.PrepareAST (prepareAST)
import CSPM.Interpreter.CoreInstances ()
import CSPM.Interpreter.Pretty ()

import Text.PrettyPrint.HughesPJClass
import System.Exit
import System.CPUTime
import Control.Exception.Base (evaluate)
import Control.Monad
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe

-- | Load a specification from a file and evaluate an expression in the context.
-- Print the result to 'stdout' and handle some exceptions.
runFile ::
     FilePath -- ^ a file containing a CSPM specification
  -> String  -- ^ a CSPM expression
  -> IO ()
runFile fileName expr
  = handleLexError lexErrorHandler
      $ handleParseError parseErrorHandler
        $ handleRenameError renameErrorHandler $
  do
    res <- evalFile False (Just fileName) expr
    putStrLn $ prettyShow $ fst res
    exitSuccess

{- Todo: clean up the mess below -}

-- | Evaluate an expression, optionall load a context from a file
evalFile ::
     Bool -- ^ verbose output ?
  -> Maybe FilePath -- ^ optional specification to load into context
  -> String -- ^ a CSPM expression
  -> IO (Value, Types.Env)
evalFile verbose context expr = do
  plainSrc <- case context of
    Just path -> readFile path
    Nothing -> return ""
  evalString verbose plainSrc (fromMaybe "expression" context) expr

-- | Evaluate an expression, optionall load a context from a string
evalString ::
     Bool -- ^ verbose output ?
  -> String -- ^ String containg CSPM specification (may be empty)
  -> String       -- ^ name of the specification for error-reporting
  -> String -- ^ a CSPM expression
  -> IO (Value, Types.Env)
evalString verbose specSrc specName expr = do
{- this is a hack:
we simply append the expression to be evaluated at the end of the sourcefile
and parse both together in one go
todo : fix
-}
  let src = specSrc ++ "\n--patch entrypoint\ntest__entry = " ++expr ++"\n"

  _startTime <- (return $ length src) >> getCPUTime
  tokenList <- lexInclude src >>= eitherToExc
  _time_have_tokens <- getCPUTime

  ast <- eitherToExc $ parse specName tokenList
  _time_have_ast <- getCPUTime

  (renamedAst, renaming) <- eitherToExc $ renameModule ast
  astNew <- evaluate $ prepareAST renamedAst
  _time_have_renaming <- getCPUTime

--  putStrLn $ "Parsing OK"
--  putStrLn $ "lextime : " ++ showTime (time_have_tokens - startTime)
--  putStrLn $ "parsetime : " ++ showTime(time_have_ast - time_have_tokens)
--  putStrLn $ "renamingtime : " ++ showTime (time_have_renaming - time_have_ast)
--  putStrLn $ "total : " ++ showTime(time_have_ast - startTime)

  time_start_execute <- getCPUTime
  let
    entry :: AST.UniqueIdent
    entry = fromJust $ Map.lookup "test__entry" $ visible renaming
    env = evalModule astNew
    val :: Value
    val = (IntMap.!) (Types.getLetBindings env) $ AST.uniqueIdentId entry
  when verbose $ do
     putStrLn $ "eval result         : " ++ prettyShow val
     time_finish_execute <- getCPUTime
     putStrLn $ "eval execution time : " ++ showTime (time_finish_execute - time_start_execute)
  return (val,env)

showTime :: Integer -> String
showTime a = show (div a 1000000000) ++ "ms"

parseErrorHandler :: ParseError -> IO ()
parseErrorHandler err = do
  putStrLn "ParseError : "
  putStrLn $ show err
  exitFailure

lexErrorHandler :: LexError -> IO ()
lexErrorHandler err = do
  putStrLn "LexError : "
  putStrLn $ show err
  exitFailure

renameErrorHandler :: RenameError -> IO ()
renameErrorHandler err = do 
  putStrLn "RenamingError : "
  putStrLn $ show err
  exitFailure
