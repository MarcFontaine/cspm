----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Assert
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD3
--
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
module CSPM.Assert
 (
  checkFileAsserts
 ,formatAssertResults
 )
where

import Language.CSPM.AST as AST
import Language.CSPM.AstUtils
import Language.CSPM.Frontend
import Language.CSPM.PrettyPrinter ()

import CSPM.Interpreter.Eval
import CSPM.Interpreter.Types (Env)
import CSPM.Interpreter (prepareAST)
import CSPM.Interpreter.CoreInstances ()

import CSPM.LTS.Deadlock (findDeadlock)

import Control.Exception.Base (evaluate)

import Text.PrettyPrint.HughesPJClass
import Data.Either

type AssertResult = Either Doc Doc

checkFileAsserts :: FilePath -> Bool -> IO [AssertResult]
checkFileAsserts src _verbose = do
  astRaw <- parseFile src
  (astR2,_) <- eitherToExc $ renameModule astRaw
  let
    astNew = prepareAST astR2
    env = evalModule $ astNew
  mapM (checkEnvAssert env) $ getModuleAsserts astNew

formatAssertResults :: [AssertResult] -> Doc
formatAssertResults results
  = vcat
    [
     text "Asserts pass :" <+> (int $ length passed)
    ,nest 4 $ vcat passed
    ,text "Asserts fail :" <+> (int $ length failed)
    ,nest 4 $ vcat failed
    ,case (length passed, length failed) of
       (0,0) -> text "No asserts found !"
       (_,0) -> text "All asserts pass !"
       (0,_) -> text "All asserts fail !"
       (_,_) -> text "Some asserts fail !"
    ]
  where
    (failed,passed) = partitionEithers results

checkEnvAssert :: Env -> LAssertDecl -> IO AssertResult
checkEnvAssert env ass = case unLabel ass of
    AssertBool expr -> if runEM (evalBool expr) env
      then return $ Right $ text "pass :" <+> pPrint ass
      else return $ Left  $ text "fail :" <+> pPrint ass
    AssertRefine {}
       -> notSupported $ text "refinement not supported yet :" <+> pPrint ass
    AssertTauPrio {}
       -> notSupported $ text "tau priority assert not supported yet :" <+> pPrint ass
    AssertModelCheck _negated _expr _property (Just ext)
       -> notSupported $ text "model checking fdr extensions :" <+> pPrint ass <+> pPrint ext
    AssertModelCheck negated expr property Nothing -> do
      let
        sigma = getSigma env
        proc = runEM (evalProcess expr) env
      res <- case unLabel property of
        DeadlockFree -> checkDeadlockFree sigma proc
        Deterministic
         -> notSupported (text "assert not supported yet :" <+> pPrint ass)
        LivelockFree
         -> notSupported (text "assert not supported yet :" <+> pPrint ass)
      return $ if negated
        then either Right Left res
        else res

  where
    notSupported = return . Left

    checkDeadlockFree sigma proc = do
      putStrLn "running BFS for deadlock state"
      res <- evaluate $ findDeadlock sigma proc
      case res of
        Nothing -> return $ Right $ pPrint ass <+> text "  ---  no deadlock found"
        Just path
           -> return $ Left $ pPrint ass <+> text "   --  deadlock found: path-length :"
               <+> (int $ length path)