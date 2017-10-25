-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.Verifier
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- A checker for the firing rules semantics of CSPM.
--
-- 'viewRuleMaybe' checks that a proof tree is valid
-- with respect to the firing rules semantics of CSPM.
-- It checks that the proof tree is syntactically valid
-- and that all side conditions hold.
-- 
-- The 'Rule' data type stores proof trees in a compressed form.
-- 'viewRuleMaybe' constructs an explicit representation of the transition.
--
-- 'viewRule' calls 'viewRuleMaybe' and throws an exception if
-- the proof tree was not valid.
-- The proof tree generators in this package only generate valid proof trees.
-- 'viewRule' is used to check that assertion.
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module CSPM.FiringRules.Verifier
  (
   viewRule
  ,viewProcBefore
  ,viewEvent
  ,viewProcAfter
  ,viewRuleMaybe
  ,viewRuleTau
  ,viewRuleTick
  ,viewRuleEvent
  )
where

import CSPM.CoreLanguage
import CSPM.CoreLanguage.Event
import CSPM.FiringRules.Rules

import Control.Monad
import Data.Maybe
import qualified Data.List as List

{-|
  This function constructs an explict representation of the transition
  from the proof tree of the transition.
  The transition as a triple
  (predecessor 'Process', Event, successor 'Process').
  If the proof tree is invalid it throws an exception.
-}
viewRule :: BL i => Rule i -> (Process i, TTE i, Process i)
viewRule proofTree = case viewRuleMaybe proofTree of
  Nothing -> error "viewRule : internal error malformed Rule"
  Just v -> v

-- | Like 'viewRule' but just return the predecessor process.
viewProcBefore :: BL i => Rule i -> Process i
viewProcBefore = (\(p,_,_) -> p) . viewRule

-- | Like 'viewRule' but just return the event.
viewEvent :: BL i => Rule i -> TTE i
viewEvent =  (\(_,e,_) -> e) . viewRule

-- | Like 'viewRule' but just return the successor process.
viewProcAfter :: BL i => Rule i  -> Process i
viewProcAfter =  (\(_,_,p) -> p) . viewRule

-- | Like 'viewRule' but returns 'Nothing' in case of an invalid proof tree.
viewRuleMaybe :: BL i => Rule i -> Maybe (Process i, TTE i, Process i)
viewRuleMaybe proofTree = case proofTree of
  TauRule r -> case viewRuleTau r of
    Just (p, p') -> Just (p, TauEvent, p')
    Nothing     -> Nothing
  TickRule r -> case viewRuleTick r of
    Just p  -> Just (p, TickEvent, Omega)
    Nothing -> Nothing
  EventRule r -> case viewRuleEvent r of
    Just (p, e, p') -> Just (p, SEvent e, p')
    Nothing -> Nothing

-- | Check a tau rule.
viewRuleTau :: forall i. BL i => RuleTau i -> Maybe (Process i, Process i)
viewRuleTau rule = case rule of
  ExtChoiceTauL pp q -> do
    (p, p') <- viewRuleTau pp
    return (ExternalChoice p q, ExternalChoice p' q)
  ExtChoiceTauR p qq -> do
    (q, q') <- viewRuleTau qq
    return (ExternalChoice p q, ExternalChoice p q')
  InternalChoiceL p q -> return (InternalChoice p q,p)
  InternalChoiceR p q -> return (InternalChoice p q,q)
  InterleaveTauL pp q -> do
    (p, p') <- viewRuleTau pp
    return (Interleave p q, Interleave p' q)
  InterleaveTauR p qq -> do
    (q, q') <- viewRuleTau qq
    return (Interleave p q, Interleave p q')
  InterleaveTickL pp q -> do
    p <- viewRuleTick pp
    return (Interleave p q, Interleave Omega q)
  InterleaveTickR p qq -> do
    q <- viewRuleTick qq
    return (Interleave p q, Interleave p Omega)
  SeqTau pp q -> do
    (p, p') <- viewRuleTau pp
    return (Seq p q, Seq p' q)
  SeqTick pp q -> do
    p <- viewRuleTick pp
    return (Seq p q, q)
  Hidden c pp -> do
    (p, e, p') <- viewRuleEvent pp
    guard $ member (undefined :: i) e c
    return (Hide c p, Hide c p')
  HideTau c pp -> do
    (p, p') <- viewRuleTau pp
    return (Hide c p, Hide c p')
  ShareTauL c pp q -> do
    (p, p') <- viewRuleTau pp
    return (Sharing p c q, Sharing p' c q)
  ShareTauR c p qq -> do
    (q, q') <- viewRuleTau qq
    return (Sharing p c q, Sharing p c q')
  ShareTickL c pp q -> do
    p <- viewRuleTick pp
    return (Sharing p c q, Sharing Omega c q)
  ShareTickR c p qq -> do
    q <- viewRuleTick qq
    return (Sharing p c q, Sharing p c Omega)
  AParallelTauL pc qc r q -> do
    (p, p') <- viewRuleTau r
    return (AParallel pc qc p q, AParallel pc qc p' q)
  AParallelTauR pc qc p r -> do
    (q, q') <- viewRuleTau r
    return (AParallel pc qc p q, AParallel pc qc p q')    
  AParallelTickL pc qc r q -> do
    p <- viewRuleTick r
    return (AParallel pc qc p q, AParallel pc qc Omega q)    
  AParallelTickR pc qc p r -> do
    q <- viewRuleTick r
    return (AParallel pc qc p q, AParallel pc qc p Omega)    
  InterruptTauL r q -> do
    (p, p') <- viewRuleTau r
    return (Interrupt p q, Interrupt p' q)
  InterruptTauR p r -> do
    (q, q') <- viewRuleTau r
    return (Interrupt p q, Interrupt p q')
  TauRepAParallel l -> do
    parts <- forM l $ \x -> case x of
      Left a -> return (a, a)
      Right (c, r) -> do 
        (p, p') <- viewRuleTau r
        return ((c,p), (c,p'))
    return (RepAParallel $ map fst parts, RepAParallel $ map snd parts)
  TimeoutTauR r q -> do
    (p, p') <- viewRuleTau r
    return (Timeout p q, Timeout p' q)
  TimeoutOccurs p q -> return (Timeout p q, q)
  RenamingTau rel pp -> do
    (p, p') <- viewRuleTau pp
    return (Renaming rel p, Renaming rel p')
  ChaosStop e -> return (Chaos e, Stop)
  LinkTauL rel pp q -> do
    (p, p') <- viewRuleTau pp
    return (LinkParallel rel p q, LinkParallel rel p' q)
  LinkTauR rel p qq -> do
    (q, q') <- viewRuleTau qq
    return (LinkParallel rel p q, LinkParallel rel p q')
  LinkTickL rel pp q -> do
    p <- viewRuleTick pp
    return (LinkParallel rel p q, LinkParallel rel Omega q)
  LinkTickR rel p qq -> do
    q <- viewRuleTick qq
    return (LinkParallel rel p q, LinkParallel rel p Omega)
  LinkLinked rel pp qq -> do
    (p, e1, p') <- viewRuleEvent pp
    (q, e2, q') <- viewRuleEvent qq
    guard $ isInRenaming (undefined :: i) rel e1 e2
    return (LinkParallel rel p q, LinkParallel rel p' q')
  TraceSwitchOn p -> return (p, p)

-- | Check a tick rule.
viewRuleTick :: BL i => RuleTick i -> Maybe (Process i)
viewRuleTick rule = case rule of
  InterleaveOmega -> return (Interleave Omega Omega)
  HiddenTick c pp -> do
    p <- viewRuleTick pp
    return $ Hide c p
  ShareOmega c -> return $ Sharing Omega c Omega
  AParallelOmega c1 c2 -> return $ AParallel c1 c2 Omega Omega
  SkipTick -> return Skip
  ExtChoiceTickL pp q -> do
    p <- viewRuleTick pp
    return $ ExternalChoice p q
  ExtChoiceTickR p qq -> do
    q <- viewRuleTick qq
    return $ ExternalChoice p q
  InterruptTick pp q -> do
    p <- viewRuleTick pp
    return $ Interrupt p q
  TimeoutTick pp q -> do
    p <- viewRuleTick pp
    return $ Timeout p q
  RepAParallelOmega l
    -> return $ RepAParallel $ zip l $ repeat Omega
  RenamingTick rel pp -> do
    p <- viewRuleTick pp
    return $ Renaming rel p
  LinkParallelTick rel
    -> return $ LinkParallel rel Omega Omega

-- | Check a regular rule
viewRuleEvent :: forall i. BL i
  => RuleEvent i -> Maybe (Process i, Event i, Process i)
viewRuleEvent rule = case rule of
  HPrefix e p -> do
    p' <- prefixNext p e
    return (Prefix p, e, p')
  ExtChoiceL pp q -> do
    (p, e, p') <- viewRuleEvent pp
    return (ExternalChoice p q, e, p')
  ExtChoiceR p qq -> do
    (q, e, q') <- viewRuleEvent qq
    return (ExternalChoice p q, e, q')
  InterleaveL pp q -> do
    (p, e, p') <- viewRuleEvent pp
    return (Interleave p q, e, Interleave p' q)
  InterleaveR p qq -> do
    (q, e, q') <- viewRuleEvent qq
    return (Interleave p q, e, Interleave p q')
  SeqNormal pp q -> do
    (p, e, p') <- viewRuleEvent pp
    return (Seq p q, e, Seq p' q)
  NotHidden c pp -> do
    (p, e, p') <- viewRuleEvent pp
    not_in_Closure e c
    return (Hide c p, e, Hide c p')
  NotShareL c pp q -> do
    (p, e, p') <- viewRuleEvent pp
    not_in_Closure e c
    return (Sharing p c q, e, Sharing p' c q)
  NotShareR c p qq -> do
    (q, e, q') <- viewRuleEvent qq
    not_in_Closure e c
    return (Sharing p c q, e, Sharing p c q')
  Shared c pp qq -> do
    (p, e1, p') <- viewRuleEvent pp
    (q, e2, q') <- viewRuleEvent qq
    guard $ eventEq ty e1 e2
    in_Closure e1 c
    return (Sharing p c q, e1, Sharing p' c q')
  AParallelL c1 c2 pp q -> do
    (p, e, p') <- viewRuleEvent pp
    in_Closure e c1
    not_in_Closure e c2
    return (AParallel c1 c2 p q, e, AParallel c1 c2 p' q)
  AParallelR c1 c2 p qq -> do
    (q, e, q') <- viewRuleEvent qq
    not_in_Closure e c1
    in_Closure e c2
    return (AParallel c1 c2 p q, e, AParallel c1 c2 p q')
  AParallelBoth c1 c2 pp qq -> do
    (p, e2, p') <- viewRuleEvent pp
    (q, e1, q') <- viewRuleEvent qq
    guard $ eventEq ty e1 e2
    in_Closure e1 c1
    in_Closure e1 c2
    return (AParallel c1 c2 p q, e1, AParallel c1 c2 p' q')
  NoInterrupt pp q -> do
    (p, e, p') <- viewRuleEvent pp
    return (Interrupt p q, e, Interrupt p' q)
  InterruptOccurs p qq -> do
    (q, e, q') <- viewRuleEvent qq
    return (Interrupt p q, e, q')
  TimeoutNo pp q -> do
    (p, e, p') <- viewRuleEvent pp
    return (Timeout p q, e, p')
  RepAParallelEvent l -> checkRepAParallel l
  Rename rel visibleEvent pp -> do
    (p, internalEvent, p') <- viewRuleEvent pp
    guard $ isInRenaming ty rel internalEvent visibleEvent
    return (Renaming rel p, visibleEvent, Renaming rel p')
  RenameNotInDomain rel pp -> do
    (p, e, p') <- viewRuleEvent pp
    guard $ not $ isInRenamingDomain ty e rel
    return (Renaming rel p, e, Renaming rel p')
  ChaosEvent c e -> do
    in_Closure e c
    return (Chaos c, e, Chaos c)
  LinkEventL rel pp q -> do
    (p, e, p') <- viewRuleEvent pp
    guard $ not $ isInRenamingDomain ty e rel
    return (LinkParallel rel p q, e, LinkParallel rel p' q)
  LinkEventR rel p qq -> do
    (q, e, q') <- viewRuleEvent qq
    guard $ not $ isInRenamingRange ty e rel
    return (LinkParallel rel p q, e, LinkParallel rel p q')
  NoException c pp q -> do
    (p, e, p') <- viewRuleEvent pp
    not_in_Closure e c
    return (Exception c p q, e, Exception c p' q)
  ExceptionOccurs c p qq -> do
    (q, e, q') <- viewRuleEvent qq
    in_Closure e c
    return (Exception c p q, e, q')
  where
    ty = (undefined :: i)
    in_Closure e c = guard $ member ty e c
    not_in_Closure e c = guard $ not $ member ty e c

    checkRepAParallel :: [EventRepAPart i] -> Maybe (Process i,Event i,Process i)
    checkRepAParallel l = do
      parts <- forM l $ \x -> case x of
        Left w -> return $ Left w
        Right (c,r) -> do { v <- viewRuleEvent r; return $ Right (c,v) }
--    Check that all events are equal.
      let events = flip mapMaybe parts $ \x -> case x of
            Left _ -> Nothing
            Right (_,(_,e,_)) -> Just e
      guard $ (not $ List.null events) 
         && (all (eventEq ty $ head events) $ tail events)
{-
Check that if the event is in a closure set the corresponding process has
also taken part in the event.
-}
      let event = head events
      guard $ flip all parts $ \x -> case x of
            Left (closure,_) -> not $ member ty event closure
            Right (closure,_) -> member ty event closure
      let
        procs = flip map parts $ \x -> case x of
          Left pair -> pair
          Right (c,(p,_,_)) -> (c,p)
        procs' = flip map parts $ \x -> case x of
          Left pair -> pair
          Right (c,(_,_,p')) -> (c,p')
      return (RepAParallel procs, event, RepAParallel procs')