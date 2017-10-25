----------------------------------------------------------------------------
-- |
-- Module      :  Main.ExecCommand
-- Copyright   :  (c) Fontaine 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Comand line interface for the CSPM tools.
----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

module Main.ExecCommand
where

import Main.Args (Args(..))
import Paths_CSPM_cspm (version)

import CSPM.Interpreter (INT,interpreterVersion,getSigma,Process,Sigma,Value(..),evalFile)
import CSPM.FiringRules.Trace (trace)
import CSPM.FiringRules.HelperClasses
import CSPM.FiringRules.Version(firingRulesVersion)
import CSPM.CoreLanguage (coreLanguageVersion)

import CSPM.LTS.MkLtsPar (mkLtsPar)
import CSPM.LTS.MkLtsDFS (mkLtsDFS)
import CSPM.LTS.ToCsp (ltsToCsp)
import CSPM.LTS.ToDot (mkDotFile)
import CSPM.Assert (checkFileAsserts, formatAssertResults)
import CSPM.Lua (runLua)

import Language.CSPM.Frontend
  (parseFile, frontendVersion
  ,eitherToExc, renameModule, castModule, lexInclude)
import Language.CSPM.LexHelper (unicodeTokenString,asciiTokenString)
import Language.CSPM.PrettyPrinter ()

import Language.CSPM.TranslateToProlog (translateToProlog,toPrologVersion)
import Language.CSPM.AstToXML (moduleToXML, showTopElement)

import System.Console.CmdArgs (isLoud) -- todo: fix this

import Text.PrettyPrint.HughesPJClass
import qualified System.Timeout as Timeout
import Control.Exception (evaluate)
import System.Exit (exitSuccess)
import System.IO
import Data.Version (showVersion)
import Control.Monad
import Data.Maybe
import Data.List as List

instance EqOrd INT
instance CSP1 INT
instance CSP2 INT
instance FShow INT

-- | execute the command according to command line arguments
execCommand :: Args -> IO ()
execCommand Info {..} = do
  putStr $ concat
    [
     "Versions :",nl
    ,"  cspm command line utility : ", showVersion version, nl
    ,"  CSPM-Frontend             : ", showVersion frontendVersion, nl
    ,"  CSPM-CoreLanguage         : ", showVersion coreLanguageVersion, nl
    ,"  CSPM-FiringRules          : ", showVersion firingRulesVersion, nl
    ,"  CSPM-Interpreter          : ", showVersion interpreterVersion, nl
    ,"  CSPM-ToProlog             : ", showVersion toPrologVersion, nl
    ,nl
    ,"Usage examples:",nl
    ,"  cspm --help",nl
    ,"  cspm eval --help",nl
    ,"  cspm info",nl
    ,"  cspm eval '3+2'",nl
    ,nl
    ,"Copyright (c) Marc Fontaine 2007-2014",nl
    ,"Source code available at: http://hackage.haskell.org/package/CSPM-cspm",nl
    ,"Email : Marc.Fontaine@gmx.de",nl
    ]
  where nl = "\n"

execCommand Lua {..} = do
  src <- readFile file
  runLua src file rest
execCommand Translate {..} = do
  when (null $ catMaybes
     [prologOut, xmlOut, prettyOut, addUnicode, removeUnicode]) $ do
    putStrLn "No output option is set"
    putStrLn "Set '--xmlOut', '--prettyOut' or an other output option"
  when (isJust xmlOut || isJust prettyOut) $ do
    -- AST transformations
      ast <- do
          ast1 <- parseFile src
          if rename
            then fmap fst $ eitherToExc $ renameModule ast1
            else return $ castModule ast1
      whenJust xmlOut $ \outFile -> do
          writeFile outFile $ showTopElement $ moduleToXML ast
      whenJust prettyOut $ \outFile -> do
          writeFile outFile $ prettyShow ast
  when (isJust addUnicode || isJust removeUnicode) $ do
    -- Token stream transformations
      tokens <- readFile src >>= lexInclude >>= eitherToExc
      whenJust addUnicode $ \outFile -> withFile outFile WriteMode
         $ \handle -> do
             hSetEncoding handle utf8
             hPutStr handle $ List.concatMap unicodeTokenString tokens
             hClose handle
      whenJust removeUnicode $ \outFile -> do
          writeFile outFile $ List.concatMap asciiTokenString tokens
  whenJust prologOut $ \outFile -> do
      translateToProlog src outFile -- translateToProlog does not return !
      error "unreachable"
  where
    whenJust a action = case a of
      Just v -> action v
      Nothing -> return ()

execCommand Eval {..} = do
  (val,_) <- evalFile verbose evalContext evalExpr
  putStrLn $ prettyShow val

execCommand Trace {..} = do
  (proc,sigma) <- mkProcess src entry
  trace sigma proc

execCommand Assert {..} = do
  results <- checkFileAsserts src verbose
  putStrLn $ show $ formatAssertResults results

execCommand LTS {..} = do
  (proc,sigma) <- mkProcess src entry
  lts <- if dfs then do
         (res,complete) <- mkLtsDFS True timeout sigma proc
         when (not complete) $ putStrLn "timeout occured: LTS only partial"
         return res
    else do
         let tout = case timeout of
               Nothing -> (-1)
               Just seconds -> round $ seconds * 1000000
         res <- Timeout.timeout tout $ evaluate $ mkLtsPar sigma proc
         case res of
            Nothing -> do
              putStrLn "timeout occured: no output will be generated"
              exitSuccess
            Just x -> return x

  case fdrOut of
    Nothing -> return ()
    Just outFile -> writeFile outFile $ render $ ltsToCsp proc lts
  case dotOut of
    Nothing -> return ()
    Just outFile -> mkDotFile outFile lts
  return ()

mkProcess :: FilePath -> String -> IO (Process, Sigma)
mkProcess file expr = do
  isVerbose <- isLoud
  (proc, env) <- evalFile isVerbose (Just file) expr
  case proc of
    VProcess p -> return (p, getSigma env)
    _ -> error "type-error : entry-point is not a process"
