{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.HelperClasses
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Some helper classes.
-- (Might be deleted or moved somewhere else some time.)
--
-----------------------------------------------------------------------------

module CSPM.FiringRules.HelperClasses
where

import CSPM.CoreLanguage
import CSPM.FiringRules.Rules

-- | Implementation i supports 'Eq' and 'Ord'.
class
  (Eq (Process i), Eq (RuleTick i), Eq (RuleTau i), Eq (RuleEvent i)
  ,Eq (EventSet i), Eq (ExtProcess i), Eq (Prefix i), Eq (Event i), Eq (RenamingRelation i)
  ,Ord (Process i), Ord (RuleTick i), Ord (RuleTau i) ,Ord (RuleEvent i)
  ,Ord (EventSet i), Ord (ExtProcess i), Ord (Prefix i), Ord (Event i), Ord (RenamingRelation i))

  => EqOrd i

-- | Implementation i supports 'Show'.
class
  (Show (TTE i), Show (Rule i), Show (Process i), Show (RuleTick i)
  , Show (RuleTau i), Show (RuleEvent i))
  => FShow i

-- | 'CSP1' means that implementation i supports the base language.
class (EqOrd i,BL i) => CSP1 i

-- | 'CSP2' means that implementation i supports the base language and multi-field events.
class (EqOrd i,BF i,CSP1 i) => CSP2 i

