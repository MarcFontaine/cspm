-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.CoreLanguage.Process
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Wrappers for the constructors of data type 'Process' and some
-- rewriting rules for replicated operations.
--
-- This can also be used as EDSL for CSP.
--
-----------------------------------------------------------------------------

module CSPM.CoreLanguage.ProcessWrapper
where

import CSPM.CoreLanguage.Process
import CSPM.CoreLanguage.Event

prefix :: Prefix i -> Process i
prefix = Prefix

externalChoice :: Process i -> Process i -> Process i
externalChoice = ExternalChoice

internalChoice :: Process i -> Process i -> Process i
internalChoice = InternalChoice

interleave :: Process i -> Process i -> Process i
interleave = Interleave

interrupt :: Process i -> Process i -> Process i
interrupt = Interrupt

timeout :: Process i -> Process i -> Process i
timeout = Timeout

sharing :: Process i -> EventSet i -> Process i -> Process i
sharing = Sharing

aparallel ::
     EventSet i -> EventSet i 
  -> Process i -> Process i
  -> Process i
aparallel = AParallel

seq :: Process i -> Process i -> Process i
seq = Seq

hide :: EventSet i -> Process i -> Process i
hide = Hide

stop :: Process i
stop = Stop

skip :: Process i
skip = Skip

switchedOff  :: ExtProcess i -> Process i
switchedOff = SwitchedOff

renaming :: RenamingRelation i -> Process i -> Process i
renaming = Renaming

linkParallel :: RenamingRelation i -> Process i -> Process i -> Process i
linkParallel = LinkParallel

repSeq :: [Process i] -> Process i
repSeq = foldr CSPM.CoreLanguage.ProcessWrapper.seq  skip

{- todo: create balanced trees of operators instead of list -}
repInternalChoice :: [Process i] -> Process i
repInternalChoice [] = stop
repInternalChoice l = foldr1 internalChoice l

repExternalChoice :: [Process i] -> Process i
repExternalChoice [] = stop
repExternalChoice l = foldr1 externalChoice l

repInterleave :: [Process i] -> Process i
repInterleave = foldr interleave skip

repAParallel :: [(EventSet i,Process i)] -> Process i
repAParallel l = case l of
  [] -> error "ProcessWrapper.hs: empty repAParallel"
  [(_,p)] -> p
  _ -> RepAParallel l

repSharing :: EventSet i -> [Process i] -> Process i
repSharing _ [] = error "ProcessWrapper.hs: empty repSharing"
repSharing _ [p] = p
repSharing c l = foldr1 (\a b -> sharing a c b) l

repLinkParallel :: RenamingRelation i -> [Process i] -> Process i
repLinkParallel _ [] = error "ProcessWrapper.hs: empty repLinkParallel"
repLinkParallel _ [_]
  = error "ProcessWrapper.hs: repLinkParallel over one process"
repLinkParallel rel l = foldr1 (\a b -> linkParallel rel a b) l

chaos :: EventSet i -> Process i
chaos = Chaos
