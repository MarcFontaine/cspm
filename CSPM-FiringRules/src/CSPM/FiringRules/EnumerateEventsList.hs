-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.EnumerateEventsList
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Reexport of the functions from EnumerateEvents with a List interface.
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
module CSPM.FiringRules.EnumerateEventsList
(
  computeTransitions
 ,eventTransitions
 ,tauTransitions
 ,tickTransitions
)
where

import CSPM.CoreLanguage
import CSPM.CoreLanguage.Event
import CSPM.FiringRules.Rules
import qualified CSPM.FiringRules.EnumerateEvents as EE
import CSPM.FiringRules.Search (runSearch)

-- | Compute all possible transitions (via an event from Sigma) for a Process.
computeTransitions ::  forall i. BL i 
  => Sigma i -> Process i -> [Rule i]
computeTransitions events p
  = runSearch $ EE.computeTransitions events p

eventTransitions :: forall i. BL i
  => Sigma i -> Process i -> [RuleEvent i]
eventTransitions sigma p = runSearch $ EE.eventTransitions sigma p

tauTransitions :: forall i. BL i => Process i -> [RuleTau i]
tauTransitions = runSearch . EE.tauTransitions

tickTransitions :: BL i => Process i -> [RuleTick i]
tickTransitions = runSearch . EE.tickTransitions
