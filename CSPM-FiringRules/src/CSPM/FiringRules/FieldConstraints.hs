-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.FieldConstraintsList
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Reexport of the functions from FieldConstraintsSearch with a List interface.
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
module CSPM.FiringRules.FieldConstraints
(
  computeTransitions
 ,eventTransitions
 ,tauTransitions
 ,tickTransitions
)
where

import CSPM.CoreLanguage
import CSPM.FiringRules.Rules
import qualified CSPM.FiringRules.FieldConstraintsSearch as FC
import CSPM.FiringRules.Search (runSearch)

-- | Compute all possible transitions of the process.
computeTransitions ::  forall i. BF i 
  => Sigma i -> Process i -> [Rule i]
computeTransitions events p
  = runSearch $ FC.computeTransitions events p

-- | Compute all (event)- transitions of the process.
eventTransitions :: forall i. BF i
  => Sigma i -> Process i -> [RuleEvent i]
eventTransitions sigma p = runSearch $ FC.eventTransitions sigma p

-- | Compute all tau-transitions of the process
tauTransitions :: forall i. BF i => Process i -> [RuleTau i]
tauTransitions = runSearch . FC.tauTransitions

-- | Compute all tick-transitions of the process
tickTransitions :: BF i => Process i -> [RuleTick i]
tickTransitions = runSearch . FC.tickTransitions
