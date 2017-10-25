----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.LTS.Deadlock
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Search the state-space of a CSPM process for deadlock states
----------------------------------------------------------------------------
module CSPM.LTS.Deadlock
(
  findDeadlock
)
where

import CSPM.CoreLanguage
import CSPM.FiringRules.Rules
import CSPM.FiringRules.Verifier (viewProcAfter)
import CSPM.FiringRules.FieldConstraints (computeTransitions)

import CSPM.Interpreter (INT, Digest)
import CSPM.Interpreter.Hash

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import qualified Data.Set as Set
import Data.Set (Set)

type Path = ([Rule INT], Process INT)

type BM a = StateT (Set Digest) (Either [Rule INT]) a

-- | Search the statespace of a process for deadlock states.
-- returns the shortes trace to a deadlock.
findDeadlock ::
     Sigma INT  -- ^ Sigma
  -> Process INT -- ^ the Process
  -> Maybe [Rule INT] -- a trace to a deadlock state (of Nothing)

findDeadlock sigma process
  = case evalStateT (wave [([],process)]) Set.empty of
      Right () -> Nothing
      Left p -> Just p
  where
    wave :: [Path] -> BM ()
    wave [] = return ()
    wave w = mapM expandPath w >>= wave . concat

    expandPath :: Path -> BM [Path]
    expandPath (path, node) = do
      t <- isOldNode node
      if t then return []
      else do
        let rules = computeTransitions sigma node
        when (null rules) $ lift $ Left $ reverse path
        return $ map (\r -> (r:path , viewProcAfter r)) rules

    isOldNode :: Process INT -> BM Bool
    isOldNode n = do
      let h = hash n
      s <- get
      let res = Set.member h s
      put $ Set.insert h s
      return res
