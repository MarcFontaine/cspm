-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.EnumerateEvents
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Brute-force computation of all possible transitions of a process.
-- Enumerates all events in 'Sigma'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
module CSPM.FiringRules.EnumerateEvents
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
import CSPM.FiringRules.Search

import Control.Monad
import Data.Either as Either
import Data.List as List


-- | Compute all possible transitions (via an event from Sigma) for a process.
computeTransitions ::  forall i. BL i 
  => Sigma i -> Process i -> Search (Rule i)
computeTransitions events p
  = (liftM EventRule $ eventTransitions events p)
         `mplus` (liftM TickRule $ tickTransitions p)
         `mplus` (liftM TauRule $ tauTransitions p)

eventTransitions :: forall i.
     BL i
  => Sigma i
  -> Process i 
  -> Search (RuleEvent i)
eventTransitions sigma p = do
  e <- anyEvent ty sigma
  buildRuleEvent e p
  where
    ty = (undefined :: i)

anyEvent :: forall i. BL i => i -> EventSet i -> Search (Event i)
anyEvent ty sigma
  = anyOf $ eventSetToList ty sigma

buildRuleEvent :: forall i. BL i => Event i -> Process i -> Search (RuleEvent i)
buildRuleEvent event proc = case proc of
  SwitchedOff p -> rp $ switchOn p
  Prefix p  -> case (prefixNext p event :: Maybe (Process i)) of
    Nothing -> mzero
    Just _ -> return $ HPrefix event p
  ExternalChoice p q
    ->       (ExtChoiceL <$> rp p <*> pure q)
     `mplus` (ExtChoiceR p <$> rp q) 
  InternalChoice _ _ -> mzero
  Interleave p q
    ->       (InterleaveL <$> rp p <*> pure q)
     `mplus` (InterleaveR p <$> rp q)
  Interrupt p q -> (NoInterrupt <$> rp p <*> pure q)
     `mplus` (InterruptOccurs p <$> rp q)
  Timeout p q -> TimeoutNo <$> rp p <*> pure q
  Sharing p c q -> if member ty event c
      then Shared c <$> rp p <*> rp q
      else (NotShareL c <$> rp p <*> pure q)
           `mplus` (NotShareR c p <$> rp q)
  Seq p q -> SeqNormal <$> rp p <*> pure q
  AParallel x y p q -> case (member ty event x, member ty event y) of
    (True, True) ->  AParallelBoth x y <$> rp p <*> rp q
    (True, False) -> AParallelL x y <$> rp p <*> pure q
    (False, True) -> AParallelR x y p <$> rp q
    (False,False) -> mzero
  RepAParallel l -> buildRuleRepAParallel event l
  Hide c p -> if member ty event c
      then mzero
      else NotHidden c <$> rp p
  Stop -> mzero
  Skip -> mzero
  Omega -> mzero
  AProcess _n -> mzero
  Renaming rel p -> (do
    e2 <- anyEvent ty (allEvents ty)
    guard $ isInRenaming ty rel e2 event
    rule <- buildRuleEvent e2 p
    return $ Rename rel event rule
    )
    `mplus` (do
       guard $ not $ isInRenamingDomain ty event rel
       RenameNotInDomain rel <$> rp p
       )
  Chaos c -> if member ty event c
    then return $ ChaosEvent c event
    else mzero
  LinkParallel rel p q -> (do
      guard $ not $ isInRenamingDomain ty event rel
      LinkEventL rel <$> rp p <*> pure q
    ) `mplus` (do
      guard $ not $ isInRenamingRange ty event rel
      LinkEventR rel p <$> rp q
    )
  Exception c p q -> if member ty event c
    then ExceptionOccurs c p <$> rp q
    else NoException c <$> rp p <*> pure q
  where
    rp = buildRuleEvent event
    ty = (undefined :: i)

buildRuleRepAParallel :: forall i. BL i
  => Event i 
  -> [(EventSet i, Process i)] -> Search (RuleEvent i)
buildRuleRepAParallel event l = do
  l2 <- mapM parPart l
  if List.null $ Either.rights l2
    then mzero
    else return $ RepAParallelEvent l2
  where
    parPart c@(alpha, p) = if member ty event alpha
      then do
        r <- buildRuleEvent event p
        return $ Right (alpha, r)
      else return $ Left c
    ty = (undefined :: i)

tauTransitions :: forall i. BL i => Process i -> Search (RuleTau i)
tauTransitions proc = case proc of
  SwitchedOff p -> tauTransitions $ switchOn p
  Prefix {} -> mzero
  ExternalChoice p q
    ->       (ExtChoiceTauL <$> tauTransitions p <*> pure q)
     `mplus` (ExtChoiceTauR p <$> tauTransitions q)
  InternalChoice p q
    ->       (return $ InternalChoiceL p q)
     `mplus` (return $ InternalChoiceR p q)
  Interleave p q
    ->       (InterleaveTauL <$> tauTransitions p <*> pure q)
     `mplus` (InterleaveTauR p <$> tauTransitions q)
     `mplus` (InterleaveTickL <$> tickTransitions p <*> pure q)
     `mplus` (InterleaveTickR p <$> tickTransitions q)
  Interrupt p q
    ->       (InterruptTauL <$> tauTransitions p <*> pure q)
     `mplus` (InterruptTauR p <$> tauTransitions q)
  Timeout p q
    ->       (TimeoutTauR <$> tauTransitions p <*> pure q)
     `mplus` (return $ TimeoutOccurs p q)
  Sharing p c q
    ->       (ShareTauL c <$> tauTransitions p <*> pure q)
     `mplus` (ShareTauR c p <$> tauTransitions q)
     `mplus` (ShareTickL c <$> tickTransitions p <*> pure q)
     `mplus` (ShareTickR c p <$> tickTransitions q)
  AParallel pc qc p q
    ->       (AParallelTauL pc qc <$> tauTransitions p <*> pure q)
     `mplus` (AParallelTauR pc qc p <$> tauTransitions q)
     `mplus` (AParallelTickL pc qc <$> tickTransitions p <*> pure q)
     `mplus` (AParallelTickR pc qc p <$> tickTransitions q)
  Seq p q
    ->        (SeqTau <$> tauTransitions p <*> pure q)
      `mplus` (SeqTick <$> tickTransitions p <*> pure q)
  Hide hidden p -> (do
    e <- anyEvent ty hidden
    rule <- buildRuleEvent e p
    return $ Hidden hidden rule)
   `mplus` (HideTau hidden <$> tauTransitions p)
  Stop -> mzero
  Skip -> mzero
  Omega -> mzero
  AProcess _n -> mzero
  RepAParallel l -> mzero -- TODO ! tau for replicated AParallel
  Renaming rel p -> RenamingTau rel <$> tauTransitions p
  Chaos c -> return $ ChaosStop c
  LinkParallel rel p q
    ->        (LinkTauL rel <$> tauTransitions p <*> pure q)
      `mplus` (LinkTauR rel p <$> tauTransitions q)
      `mplus` (LinkTickL rel <$> tickTransitions p <*> pure q)
      `mplus` (LinkTickR rel p <$> tickTransitions q)
      `mplus` mkLinkedRules rel p q
  Exception c p q -> mzero -- TODO
  where
    ty = (undefined :: i)

mkLinkedRules :: forall i. BL i
   => RenamingRelation i
   -> Process i
   -> Process i
   -> Search (RuleTau i)
mkLinkedRules rel p q = do
  (e1, r1) <- rules1
  (e2, r2) <- rules2
  guard $ isInRenaming ty rel e1 e2
  return $ LinkLinked rel r1 r2
  where
    rules1 :: Search (Event i, RuleEvent i)
    rules1 = rules (getRenamingDomain ty rel) p
    rules2 = rules (getRenamingRange ty rel) q
    rules :: [Event i] -> Process i -> Search (Event i, RuleEvent i)
    rules s proc = do
      e <- anyOf s
      r <- buildRuleEvent e proc
      return (e,r)
    ty = (undefined :: i)

tickTransitions :: BL i => Process i -> Search (RuleTick i)
tickTransitions proc = case proc of
  SwitchedOff p -> tickTransitions $ switchOn p
  Prefix {} -> mzero
  ExternalChoice p q
    ->       (ExtChoiceTickL <$> tickTransitions p <*> pure q)
     `mplus` (ExtChoiceTickR p <$> tickTransitions q)
  InternalChoice _p _q -> mzero
  Interleave Omega Omega -> return $ InterleaveOmega
  Interleave _ _ -> mzero
  Interrupt p q -> InterruptTick <$> tickTransitions p <*> pure q
  Timeout p q -> TimeoutTick <$> tickTransitions p <*> pure q
  Sharing Omega c Omega -> return $ ShareOmega c
  Sharing _ _ _ -> mzero
  AParallel c1 c2 Omega Omega -> return $ AParallelOmega c1 c2
  AParallel _ _ _ _ -> mzero
  Seq _p _q -> mzero
  Hide c p -> HiddenTick c <$> tickTransitions p
  Stop -> mzero
  Skip -> return SkipTick
  Omega -> mzero
  AProcess _n -> mzero
  RepAParallel l -> if all (isOmega . snd) l
    then return $ RepAParallelOmega $ map fst l
    else mzero
  Renaming rel p -> RenamingTick rel <$> tickTransitions p
  Chaos _ -> mzero
  LinkParallel rel Omega Omega -> return $ LinkParallelTick rel
  LinkParallel _ _ _ -> mzero
  Exception c p q -> mzero -- TODO
