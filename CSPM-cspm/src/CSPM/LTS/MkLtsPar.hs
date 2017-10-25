----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.LTS.mkLtsPar
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Compute the labled transition system of a process.
----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module CSPM.LTS.MkLtsPar
(
  mkLtsPar
)
where

import CSPM.CoreLanguage
import CSPM.FiringRules.Rules
import CSPM.FiringRules.Verifier (viewProcAfter)
import CSPM.FiringRules.FieldConstraints (computeTransitions)

import CSPM.Interpreter (INT)

import CSPM.LTS.LTS

import Control.Parallel.Strategies
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List as List

-- | Compute the LTS of a Process using parallel strategies
mkLtsPar :: Sigma INT -> Process INT -> LTS
mkLtsPar events process
  = wave [mkLtsNode process] Map.empty
  where
    wave :: [LtsNode] -> LTS -> LTS
    wave [] lts = lts
    wave w lts = wave (Set.toList uniqueProcesses) newLts
      where
        !transitions = parRules $ map processNext w

        processNext :: LtsNode -> (LtsNode, [Rule INT], [LtsNode])
        processNext p = (p, rules, map (mkLtsNode . viewProcAfter) rules)
          where rules =  computeTransitions events $ nodeProcess p
      
        !newLts = List.foldl' insertTransition lts transitions
          where
            insertTransition :: LTS -> (LtsNode, [Rule INT], [LtsNode]) -> LTS
            insertTransition l (p, rules, _) = Map.insert p rules l

        uniqueProcesses :: Set LtsNode
        !uniqueProcesses = List.foldl' insertProcess Set.empty processes
           where
             insertProcess :: Set LtsNode -> LtsNode -> Set LtsNode
             insertProcess s p = if Map.member p newLts
               then s
               else p `Set.insert` s

             processes = concatMap (\(_,_,r) -> r) transitions


    parRules ::
          [(LtsNode, [Rule INT], [LtsNode ])]
       -> [(LtsNode, [Rule INT], [LtsNode ])]
    parRules = withStrategy $ parList $ evalTuple3 r0 (parList rseq) (parList rseq)
 {- 
   semantic : parRules = id
 -}

{-
    parRules = withStrategy $ parList $ \(p,rl,pl) -> do
       p' <- rpar (p `using` r0)
       rl' <- rpar (rl `using` parList rwhnf)
       pl' <- rpar (pl `using` parList rwhnf)
       return (p',rl',pl')
-}