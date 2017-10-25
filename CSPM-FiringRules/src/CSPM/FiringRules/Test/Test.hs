-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.Test.Test
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- QuickCheck tests for the proof tree generators in
-- module CSPM.FiringRules.EnumerateEvents and
-- CSPM.FiringRules.FieldConstraints.
-- These QuickCheck properties check for soundness, completeness
-- and that both proof tree generators yield the same result.
--
-----------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving,FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CSPM.FiringRules.Test.Test
(
  main
)
where

import CSPM.CoreLanguage
import CSPM.CoreLanguage.Event (allEvents)

import CSPM.FiringRules.Rules
import CSPM.FiringRules.Verifier
import CSPM.FiringRules.Test.Mock1
import CSPM.FiringRules.Test.Mock2
import qualified CSPM.FiringRules.EnumerateEventsList as EnumNext
import qualified CSPM.FiringRules.FieldConstraints as FieldNext
import CSPM.FiringRules.HelperClasses

import System.Random
import Test.QuickCheck as QC
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import Control.Monad

-- | Run a number of QuickCheck tests (with fixed seed).
main :: IO ()
main = forM_ [1,2,3,4] $ \seed -> do
  putStrLn $ "\n\n\nSeed " ++ show seed
  mainDet seed

mainDet :: Int -> IO ()
mainDet i = do
  setStdGen $ mkStdGen i
  testAll

testAll :: IO ()
testAll = do
  testMock1
  testMock2
  testFields

testMock1 :: IO ()
testMock1 = do
  putStrLn "testing Mock1"
  quickCheck $ QC.label "generator Tau rules" 
    ((isJust . viewRuleTau) :: RuleTau M1 -> Bool)
  quickCheck $ QC.label "generator Tick rules"
    ((isJust . viewRuleTick) :: RuleTick M1 -> Bool)
  quickCheck $ QC.label "generator Event rules"
    ((isJust . viewRuleEvent) :: RuleEvent M1 -> Bool)
  quickCheck $ QC.label "sound enum Tick rules" 
    (sound_EnumRuleTick :: RuleTick M1 -> Bool)
  quickCheck $ QC.label "sound enum Tau rules"
    (sound_EnumRuleTau :: RuleTau M1 -> Bool)
  quickCheck $ QC.label "sound enum Event rules"
    (sound_EnumRuleEvent :: RuleEvent M1 -> Bool)
  quickCheck $ QC.label "complete enum Tick rules"
    (complete_enumTickRules :: RuleTick M1 -> Bool)
  quickCheck $ QC.label "complete enum Tau rules"
    (complete_enumTauRules :: RuleTau M1 -> Bool)
  quickCheck $ QC.label "complete enum Event rules"
    (complete_enumEventRules :: RuleEvent M1 -> Bool)

testMock2 :: IO ()
testMock2 = do
  putStrLn "\n\ntesting Mock2\n\n"
  quickCheck $ QC.label "generator Tau rules" 
    ((isJust . viewRuleTau) :: RuleTau M2 -> Bool)
  quickCheck $ QC.label "generator Tick rules"
    ((isJust . viewRuleTick) :: RuleTick M2 -> Bool)
  quickCheck $ QC.label "generator Event rules"
    ((isJust . viewRuleEvent) :: RuleEvent M2 -> Bool)
  quickCheck $ QC.label "sound enum Tick rules" 
    (sound_EnumRuleTick :: RuleTick M2 -> Bool)
  quickCheck $ QC.label "sound enum Tau rules"
    (sound_EnumRuleTau :: RuleTau M2 -> Bool)
  quickCheck $ QC.label "sound enum Event rules"
    (sound_EnumRuleEvent :: RuleEvent M2 -> Bool)
  quickCheck $ QC.label "complete enum Tick rules"
    (complete_enumTickRules :: RuleTick M2 -> Bool)
  quickCheck $ QC.label "complete enum Tau rules"
    (complete_enumTauRules :: RuleTau M2 -> Bool)
  quickCheck $ QC.label "complete enum Event rules"
    (complete_enumEventRules :: RuleEvent M2 -> Bool)
  quickCheck $ QC.label "enum Event rules == evalEventRules"
    (computeNext_eq_EnumRuleEvent :: RuleEvent M2 -> Bool)
  quickCheck $ QC.label "enum Tau rules == symRuleTau"
    (fieldTau :: RuleTau M2 -> Bool)
  quickCheck $ QC.label "enum Tick rules == symRuleTick"
    (fieldTick :: RuleTick M2 -> Bool)


sound_EnumRuleTick :: CSP1 i => RuleTick i -> Bool
sound_EnumRuleTick r
  = all (checkRule proc . TickRule) $ EnumNext.tickTransitions proc
  where proc = viewProcBefore $ TickRule r

sound_EnumRuleTau :: CSP1 i => RuleTau i -> Bool
sound_EnumRuleTau r
  = all (checkRule proc . TauRule) $ EnumNext.tauTransitions proc
  where proc = viewProcBefore $ TauRule r

sound_EnumRuleEvent :: forall i. CSP1 i => RuleEvent i -> Bool
sound_EnumRuleEvent r
  = all (checkRule proc . EventRule) $ EnumNext.eventTransitions sigma proc
  where
    proc = viewProcBefore $ EventRule r
    sigma = allEvents (undefined :: i)

checkRule :: CSP1 i => Process i -> Rule i -> Bool
checkRule proc r
  = case viewRuleMaybe r of
      Nothing -> False
      Just (p,_,_) -> p == proc

complete_enumTickRules :: CSP1 i => RuleTick i -> Bool
complete_enumTickRules r
  = r `List.elem` (EnumNext.tickTransitions $ viewProcBefore $ TickRule r)

complete_enumTauRules :: CSP1 i => RuleTau i -> Bool
complete_enumTauRules r
  = r `List.elem` (EnumNext.tauTransitions $ viewProcBefore $ TauRule r)

complete_enumEventRules :: forall i. CSP1 i => RuleEvent i -> Bool
complete_enumEventRules r
  = r `List.elem` (EnumNext.eventTransitions sigma $ viewProcBefore $ EventRule r)
  where sigma = allEvents (undefined :: i)

testFields :: IO ()
testFields = do
  putStrLn "\n\nTesting computeNext"
  quickCheck $ QC.label "sound_computeNext" 
    (sound_computeNext :: RuleEvent M2 -> Bool)

  quickCheck $ QC.label "complete_computeNext" 
    (complete_computeNext :: RuleEvent M2 -> Bool)

  quickCheck $ QC.label "FieldNext.eventTransitions == EnumNext.eventTransitions"
    (computeNext_eq_EnumRuleEvent :: RuleEvent M2 -> Bool)

  quickCheck $ QC.label "FieldNext.tauTransitions == EnumNext.tauTransitions"
    (fieldTau :: RuleTau M2 -> Bool)

  quickCheck $ QC.label "FieldNext.tickTransitions == EnumNext.tickTransitions"
    (fieldTick :: RuleTick M2 -> Bool)

sound_computeNext :: forall i. CSP2 i => RuleEvent i -> Bool
sound_computeNext r
  = all (checkRule proc . EventRule) $ FieldNext.eventTransitions sigma proc
  where
    proc = viewProcBefore $ EventRule r
    sigma = allEvents (undefined :: i)

complete_computeNext :: forall i. CSP2 i => RuleEvent i -> Bool
complete_computeNext r
  = r `List.elem` (FieldNext.eventTransitions sigma $ viewProcBefore $ EventRule r)
  where
    sigma = allEvents (undefined :: i)

computeNext_eq_EnumRuleEvent :: forall i. CSP2 i => RuleEvent i -> Bool
computeNext_eq_EnumRuleEvent rule = ruleSet1 == ruleSet2
  where
    ruleSet1 = Set.fromList $ FieldNext.eventTransitions sigma proc
    ruleSet2 = Set.fromList $ EnumNext.eventTransitions sigma proc
    proc = viewProcBefore $ EventRule rule
    sigma = allEvents (undefined :: i)

fieldTau :: forall i. CSP2 i => RuleTau i -> Bool
fieldTau rule = ruleSet1 == ruleSet2
  where
    ruleSet1 = Set.fromList $ EnumNext.tauTransitions proc
    ruleSet2 = Set.fromList $ FieldNext.tauTransitions proc
    proc = viewProcBefore $ TauRule rule

fieldTick :: forall i. CSP2 i => RuleTick i -> Bool
fieldTick rule = ruleSet1 == ruleSet2
  where
    ruleSet1 = Set.fromList $ EnumNext.tickTransitions proc
    ruleSet2 = Set.fromList $ FieldNext.tickTransitions proc
    proc = viewProcBefore $ TickRule rule
