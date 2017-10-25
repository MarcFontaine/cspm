-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.CoreLanguage.Event
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module defines the event-related part of an interface between 
-- the CSPM-CoreLanguage and the underlying implementation.
-- The underlying implementation has to instantiate the type families 'Event',
-- 'EventSet', 'RenamingRelation'
-- and the class 'BE' ('BE'== base event).
-- 
-- For full CSPM support (channels with multiple fields, event closure sets etc.)
-- CSPM.CoreLanguage.Field is also needed.
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CSPM.CoreLanguage.Event
where
import Data.Typeable

type family Event i
type family EventSet i
type family RenamingRelation i

-- | Sigma is the set of all events that appear in a system.
type Sigma i = EventSet i

-- | The first argument of all functions in 'BE' is a phantom-type-argument, i.e.
-- applications pass _|_ and implementations must not use this value.
class BE i where
  eventEq :: i -> Event i -> Event i -> Bool
  member ::  i -> Event i -> EventSet i -> Bool
  intersection :: i -> EventSet i -> EventSet i -> EventSet i
  difference :: i -> EventSet i -> EventSet i -> EventSet i
  union :: i -> EventSet i -> EventSet i -> EventSet i
  null :: i -> EventSet i -> Bool
  singleton :: i -> Event i -> EventSet i
  insert :: i -> Event i -> EventSet i -> EventSet i
  delete :: i -> Event i -> EventSet i -> EventSet i
  eventSetToList :: i -> EventSet i -> [Event i]
  allEvents :: i -> EventSet i
  isInRenaming :: i -> RenamingRelation i -> Event i -> Event i -> Bool
  imageRenaming :: i -> RenamingRelation i -> Event i -> [Event i]
  preImageRenaming :: i -> RenamingRelation i -> Event i -> [Event i]
  isInRenamingDomain :: i -> Event i -> RenamingRelation i -> Bool
  isInRenamingRange :: i -> Event i -> RenamingRelation i -> Bool
  getRenamingDomain :: i -> RenamingRelation i -> [Event i]
  getRenamingRange  :: i -> RenamingRelation i -> [Event i]
  renamingFromList :: i -> [(Event i, Event i)] -> RenamingRelation i
  renamingToList :: i -> RenamingRelation i -> [(Event i, Event i)]
  singleEventToClosureSet :: i -> Event i -> EventSet i

-- | A wrapper for tick-events, tau-events and events from Sigma.
data TTE i
  = TickEvent
  | TauEvent
  | SEvent (Event i)
  deriving Typeable

class ShowEvent i where showEvent :: i -> String
class ShowTTE i where showTTE :: i -> String
