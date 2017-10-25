-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.CoreLanguage.Process
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This modules defines an FDR-compatible CSP core language.
-- The core language deals with CSP-related constructs like processes and events.
--
-- The implementation of the underlying language
-- must provide instances for the type families 'Prefix', 'ExtProcess'
-- and class 'BL'.
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CSPM.CoreLanguage.Process
where
import Data.Typeable

import CSPM.CoreLanguage.Event

-- | A prefix expression.
type family Prefix i

-- | A process that has not yet been switched on.
type family ExtProcess i

class (BE i) => BL i where
  -- | Try to perform an 'Event' return the successor 'Process' or Nothing
  --   if the event is not possible.
  prefixNext :: Prefix i -> Event i -> Maybe (Process i)
  switchOn :: ExtProcess i -> Process i

{-|
  A data type for CSPM processes.
  For efficiency, replicated alphabetized parallel has an explicit constructor.
  Other replicated operations get translated on the fly.
  For constructing processes one should rather use the wrappers from 
  CSPM.CoreLanguage.ProcessWrappers.
-}
data Process i
  = Prefix (Prefix i)
  | ExternalChoice (Process i) (Process i)
  | InternalChoice (Process i) (Process i)
  | Interleave  (Process i) (Process i)
  | Interrupt (Process i) (Process i)
  | Timeout (Process i) (Process i)
  | Sharing (Process i) (EventSet i) (Process i)
  | AParallel (EventSet i) (EventSet i) (Process i) (Process i) 
  | RepAParallel [(EventSet i,Process i)]
  | Seq (Process i) (Process i)
  | Hide (EventSet i) (Process i)
  | Stop
  | Skip
  | Omega
  | Chaos (EventSet i)
  | AProcess Int -- ^ Just for debugging.
  | SwitchedOff  (ExtProcess i)
  | Renaming (RenamingRelation i) (Process i)
  | LinkParallel (RenamingRelation i) (Process i) (Process i)
  | Exception (EventSet i) (Process i) (Process i)
  deriving Typeable

isOmega :: Process i -> Bool
isOmega Omega = True
isOmega _ = False
