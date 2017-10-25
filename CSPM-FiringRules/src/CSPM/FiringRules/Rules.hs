-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.Rules
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module defines data types for (CSP) proof trees.
-- A proof tree shows that a particular transition is valid
-- with respect to the firing rules semantics.
--
-- (For more info on the firing rule semantics 
-- see: The Theory and Practice of Concurrency A.W. Roscoe 1999.)
-- 
-- We use three separate data types:
-- 'RuleTau' stores a proof tree for a tau rule,
-- 'RuleTick' stores a proof tree for a tick rule and
-- 'RuleEvent' stores a proof tree for an event from Sigma.
--
-- There is a one-to-one correspondence between
-- each constructor of the data types 'RuleTau', 'RuleTick', 'RuleEvent'
-- and one fireing rule.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module CSPM.FiringRules.Rules
where
import CSPM.CoreLanguage
import Data.Typeable

-- | A sum-type for tau, tick and regular proof trees.
data Rule i
  = TauRule (RuleTau i)
  | TickRule (RuleTick i)
  | EventRule (RuleEvent i)
  deriving Typeable

-- | Is this a proof tree for a tau-transition ?
isTauRule :: Rule i -> Bool
isTauRule (TauRule {}) = True
isTauRule _ = False

-- | Representation of tau proof trees.
data RuleTau i
  = Hidden (EventSet i) (RuleEvent i)
  | HideTau (EventSet i) (RuleTau i)
  | SeqTau (RuleTau i) (Process i)
  | SeqTick (RuleTick i) (Process i)
  | InternalChoiceL (Process i) (Process i)
  | InternalChoiceR (Process i) (Process i)
  | ChaosStop (EventSet i)
  | TimeoutOccurs (Process i) (Process i)
  | TimeoutTauR (RuleTau i) (Process i)
  | ExtChoiceTauL (RuleTau i) (Process i)
  | ExtChoiceTauR (Process i) (RuleTau i)
  | InterleaveTauL (RuleTau i) (Process i)
  | InterleaveTauR (Process i) (RuleTau i)
  | InterleaveTickL (RuleTick i) (Process i)
  | InterleaveTickR (Process i) (RuleTick i)
  | ShareTauL (EventSet i) (RuleTau i) (Process i)
  | ShareTauR (EventSet i) (Process i) (RuleTau i)
  | ShareTickL (EventSet i) (RuleTick i) (Process i)
  | ShareTickR (EventSet i) (Process i) (RuleTick i)
  | AParallelTauL (EventSet i) (EventSet i) (RuleTau i) (Process i)
  | AParallelTauR (EventSet i) (EventSet i) (Process i) (RuleTau i)
  | AParallelTickL (EventSet i) (EventSet i) (RuleTick i) (Process i)
  | AParallelTickR (EventSet i) (EventSet i) (Process i) (RuleTick i)
  | InterruptTauL (RuleTau i) (Process i)
  | InterruptTauR (Process i) (RuleTau i)
  | TauRepAParallel [Either (EventSet i,Process i) (EventSet i,RuleTau i)]
  | RenamingTau (RenamingRelation i) (RuleTau i)
  | LinkLinked (RenamingRelation i) (RuleEvent i) (RuleEvent i)
  | LinkTauL (RenamingRelation i) (RuleTau i) (Process i)
  | LinkTauR (RenamingRelation i) (Process i) (RuleTau i)
  | LinkTickL (RenamingRelation i) (RuleTick i) (Process i)
  | LinkTickR (RenamingRelation i)  (Process i) (RuleTick i)
  | ExceptionTauL (EventSet i) (RuleTau i) (Process i)
  | ExceptionTauR (EventSet i) (Process i) (RuleTau i)
  | TraceSwitchOn (Process i) -- pseudo-tau for debugging

-- | Representation of tick proof trees.
data RuleTick i
  = SkipTick
  | HiddenTick (EventSet i) (RuleTick i)
  | InterruptTick (RuleTick i) (Process i)
  | TimeoutTick (RuleTick i) (Process i)
  | ShareOmega (EventSet i)
  | AParallelOmega (EventSet i) (EventSet i)
  | RepAParallelOmega [EventSet i]
  | InterleaveOmega
  | ExtChoiceTickL (RuleTick i) (Process i)
  | ExtChoiceTickR (Process i) (RuleTick i)
  | RenamingTick (RenamingRelation i) (RuleTick i)
  | LinkParallelTick (RenamingRelation i)

-- | Representation of regular proof trees.
data RuleEvent i
  = HPrefix (Event i) (Prefix i)
  | ExtChoiceL (RuleEvent i) (Process i)
  | ExtChoiceR (Process i) (RuleEvent i)
  | InterleaveL (RuleEvent i) (Process i)
  | InterleaveR (Process i) (RuleEvent i)
  | SeqNormal (RuleEvent i) (Process i)
  | NotHidden (EventSet i) (RuleEvent i)
  | NotShareL (EventSet i) (RuleEvent i) (Process i)
  | NotShareR (EventSet i) (Process i) (RuleEvent i)
  | Shared (EventSet i) (RuleEvent i) (RuleEvent i)
  | AParallelL (EventSet i) (EventSet i) (RuleEvent i) (Process i)
  | AParallelR (EventSet i) (EventSet i) (Process i) (RuleEvent i)
  | AParallelBoth (EventSet i) (EventSet i) (RuleEvent i) (RuleEvent i)
  | RepAParallelEvent [EventRepAPart i]
  | NoInterrupt (RuleEvent i) (Process i)
  | InterruptOccurs (Process i) (RuleEvent i)
  | TimeoutNo (RuleEvent i) (Process i)
  | Rename (RenamingRelation i) (Event i) (RuleEvent i)
-- todo make special cases for Rename injective and rename relational
  | RenameNotInDomain (RenamingRelation i) (RuleEvent i)
  | ChaosEvent (EventSet i) (Event i)
  | LinkEventL (RenamingRelation i) (RuleEvent i) (Process i)
  | LinkEventR (RenamingRelation i) (Process i) (RuleEvent i)
  | NoException (EventSet i) (RuleEvent i)  (Process i)
  | ExceptionOccurs (EventSet i) (Process i) (RuleEvent i)

type EventRepAPart i
  = Either (EventSet i, Process i) (EventSet i, RuleEvent i)

{-
Not sure about this.
Maybe this moves somewhere else or should be implemented differently.
This is somehow complicated by the use of type families 
-}
deriving instance
  (Show (Event i), Show (Prefix i), Show (Process i), Show (ExtProcess i)
  ,Show (EventSet i), Show (RenamingRelation i))
  => Show (RuleEvent i)
deriving instance
  (Eq (Event i), Eq (Prefix i), Eq (Process i), Eq (ExtProcess i)
  ,Eq (EventSet i), Eq (RenamingRelation i) )
  => Eq (RuleEvent i)
deriving instance
  (Ord (Event i), Ord (Prefix i), Ord (Process i), Ord (ExtProcess i)
  ,Ord (EventSet i), Ord (RenamingRelation i) )
  => Ord (RuleEvent i)


deriving instance
  (Show (Process i), Show (EventSet i), Show (Prefix i), Show (ExtProcess i)
  ,Show (RenamingRelation i))
  => Show (RuleTick i)
deriving instance
  (Eq (Process i), Eq (EventSet i), Eq (Prefix i), Eq (ExtProcess i)
  ,Eq (RenamingRelation i))
  => Eq (RuleTick i)
deriving instance
  (Ord (Process i), Ord (EventSet i), Ord (Prefix i), Ord (ExtProcess i)
  ,Ord (RenamingRelation i))
   => Ord (RuleTick i)


deriving instance
  (Show (RuleEvent i), Show (RuleTick i), Show (Process i)
  ,Show (EventSet i), Show (RenamingRelation i))
  => Show (RuleTau i)
deriving instance
  (Eq (RuleEvent i), Eq (RuleTick i), Eq (Process i)
  ,Eq (EventSet i), Eq (RenamingRelation i))
  => Eq (RuleTau i)
deriving instance
  (Ord (RuleEvent i), Ord (RuleTick i), Ord (Process i), Ord (EventSet i)
  ,Ord (ExtProcess i), Ord (Prefix i), Ord (Event i),Ord (RenamingRelation i) )
  => Ord (RuleTau i)

deriving instance
  (Show (RuleEvent i), Show (RuleTick i), Show (RuleTau i))
  => Show (Rule i)

deriving instance
  (Eq (RuleEvent i), Eq (RuleTick i), Eq (RuleTau i))
  => Eq (Rule i)
