----------------------------------------------------------------------------
-- |
-- Module      :  Main.Args
-- Copyright   :  (c) Fontaine 2010-2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Argument parser for the command line interface
----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main.Args
(
  Args(..)
 ,argParser
)
where

import System.Console.CmdArgs
import Paths_CSPM_cspm (version)
import Data.Version (showVersion)

-- | Command line argument parser using cmdargs library.
argParser :: Mode (CmdArgs Args)
argParser = cmdArgsMode
  $ modes [
        infoMode, evalMode, traceMode, assertMode, ltsMode
       , translateMode, luaMode]
  &= program "cspm"
  &= summary ("cspm command line utility version " ++ showVersion version)

data Args =
   Info {
     verbose :: Bool
    }
  |Eval {
     evalContext :: Maybe FilePath
    ,evalExpr :: String
    ,verbose :: Bool
    }
  |Trace {
     src    :: FilePath
    ,entry  :: String
    ,verbose :: Bool
    }
  |LTS {
     src    :: FilePath
    ,timeout:: Maybe Double
    ,dfs    :: Bool
    ,entry  :: String
    ,fdrOut :: Maybe FilePath
    ,dotOut :: Maybe FilePath
    ,verbose :: Bool
    }
  |Assert {
     src    :: FilePath
    ,verbose :: Bool
    }
  |Translate {
     src    :: FilePath
    ,rename :: Bool
    ,xmlOut :: Maybe FilePath
    ,prettyOut     :: Maybe FilePath
    ,addUnicode    :: Maybe FilePath
    ,removeUnicode :: Maybe FilePath
    ,prologOut     :: Maybe FilePath
    }
  |Lua {
     file    :: FilePath
    ,rest   :: [String]
    }
   deriving (Data,Typeable,Show,Eq)

infoMode :: Args
infoMode = Info {
   verbose = def
      &= help "verbose"
      &= name "v" &= name "verbose"
  } &= details ["print some information about the program"] &= auto

evalMode :: Args
evalMode = Eval {
   evalContext = def
--       &= help "optional: CSPM specification to load into context"
       &= typFile
       &= explicit &= name "s" &= name "src"
  ,evalExpr = def
       &= argPos 0
       &= typ "EXPR"
--       &= help "the expression to evaluate"
  ,verbose = def
      &= help "verbose"
      &= name "v" &= name "verbose"
  } &= details ["evaluate an expression"]

traceMode :: Args
traceMode = Trace {
  src = def
    &= argPos 0
    &= typFile
--    &= help "CSPM specification"
  ,entry = "MAIN"
    &= help "optional: the main process"
    &= typ "PROCESS"
    &= explicit &= name "main" &= name "m"
  ,verbose = def
      &= help "verbose"
      &= name "v" &= name "verbose"
  } &= details ["trace a process"]

ltsMode :: Args
ltsMode = LTS {
  src = def
--    &= help "CSPM specification"
    &= typFile
    &= argPos 0
  ,entry = "MAIN"
    &= help "optional: the main process"
    &= typ "PROCESS"
    &= explicit &= name "main" &= name "m"
  ,timeout = def
    &= help "optional: timeout in seconds"
    &= typ "DOUBLE"
    &= explicit &= name "timeout"
  ,dfs = def
    &= help "use DFS algorithm (can compute a partial LTS)"
    &= explicit &= name "dfs" &= name "partial"
  ,dotOut = def
    &= help "optional: write output-file in dot format"
    &= typFile
    &= explicit &= name "dotOut"
  ,fdrOut = def
    &= help "optional: write output-file suitable for fdr refinement checking"
    &= typFile
    &= explicit &= name "fdrOut"
  ,verbose = def
      &= help "verbose"
      &= name "v" &= name "verbose"
  } &= details ["compute the LTS and dump it in various formats"]

luaMode :: Args
luaMode = Lua {
   file = def
    &= typFile
    &= argPos 0
  ,rest = def &= args
  } &= details ["run a lua script"]

assertMode :: Args
assertMode = Assert {
  src = def
    &= typFile
    &= argPos 0
  ,verbose = def
      &= help "verbose"
      &= name "v" &= name "verbose"
  } &= details ["check the assert declarations of a specification"]

translateMode :: Args
translateMode = Translate {
   src = def
     &= typFile
     &= argPos 0
  ,rename = False
     &= help "run renaming  on the AST"
     &= explicit &= name "rename"
  ,xmlOut = def
     &= help "optional: write a file with containing XML"
     &= typFile
     &= explicit &= name "xmlOut"
  ,addUnicode = def
     &= help "optional: replace some CSPM symbols with unicode"
     &= typFile
     &= explicit &= name "addUnicode"
  ,removeUnicode = def
     &= help "optional: replace some unicode symbols with default CSPM encoding"
     &= typFile
     &= explicit &= name "removeUnicode"
  ,prettyOut = def
     &= help "optional: prettyPrint to a file"
     &= typFile
     &= explicit &= name "prettyOut"
  ,prologOut = def
     &= help "translate a CSP-M file to Prolog"
     &= typFile
     &= explicit &= name "prologOut"
  } &= details ["Parse a specification and write the parse result to a file."]
