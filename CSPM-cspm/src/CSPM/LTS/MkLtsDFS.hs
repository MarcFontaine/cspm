----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.LTS.mkLtsDFS
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Compute the labled transition system of a process.
-- Uses depth first search and runs in the IO-Monad.
-- A Timeout can be set and the function can return a partial LTS
----------------------------------------------------------------------------
module CSPM.LTS.MkLtsDFS
(
  dfsStep
 ,mkLtsDFS
)
where

import CSPM.CoreLanguage
import CSPM.FiringRules.Rules
import CSPM.FiringRules.Verifier (viewProcAfter)
import CSPM.FiringRules.FieldConstraints (computeTransitions)

import CSPM.Interpreter (INT)

import CSPM.LTS.LTS

import qualified Data.Map as Map

import System.Timeout as Timeout
import Control.Exception
import Control.Monad
import Data.IORef

-- | Generate an LTS with a DFS
{-# WARNING mkLtsDFS "mkLts leaks memory: TODO : fix this" #-}
mkLtsDFS :: Bool -> Maybe Double -> Sigma INT -> Process INT -> IO (LTS, Bool)
mkLtsDFS progressReport maxTime sigma process = do
  ltsPtr <- newIORef ([mkLtsNode process],Map.empty)
  let tout = case maxTime of
        Nothing -> (-1)
        Just seconds -> round $ seconds * 1000000
  res <- Timeout.timeout tout $ dfsLoop ltsPtr sigma
  case res of
    Nothing -> do
      state <- readIORef ltsPtr
      return (snd state, False)
    Just () -> do
      state <- readIORef ltsPtr
      return (snd state, True)


dfsLoop :: IORef DFSState -> Sigma INT -> IO ()
dfsLoop ltsPtr sigma = loop
  where
    loop = do
      lts <- fmap snd $ readIORef ltsPtr
      void $ evaluate lts
      finish <- atomicModifyIORef ltsPtr $ dfsStep sigma
      if finish then return () else loop

type DFSState = ([LtsNode],LTS)

-- | perform one iteration of the DFS loop
dfsStep :: Sigma INT -> DFSState -> (DFSState,Bool)
dfsStep _ ([],lts) = (([],lts),True)
dfsStep sigma (proc:restQueue,lts)
  = if proc `Map.member` lts
      then ((restQueue,lts),False)
      else ((newQueue,newLts),False)
  where
    transitions :: [Rule INT]
    transitions = computeTransitions sigma $ nodeProcess proc
    newNodes = map (mkLtsNode . viewProcAfter) transitions
    newLts = Map.insert proc transitions lts
    newQueue = newNodes ++ restQueue
