-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.CoreLanguage.Field
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module defines the class 'BF' for versions of CSP
-- that also support multi-field-events and event-closure sets.
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}

module CSPM.CoreLanguage.Field
where

import CSPM.CoreLanguage.Event
import CSPM.CoreLanguage.Process

type family Field i
type family FieldSet i
type family ClosureState i
type family PrefixState i

class BL i => BF i where
  fieldEq :: i -> Field i -> Field i -> Bool
  member :: i -> Field i -> FieldSet i -> Bool
  intersection :: i -> FieldSet i -> FieldSet i -> FieldSet i
  difference :: i -> FieldSet i -> FieldSet i -> FieldSet i
  union :: i -> FieldSet i -> FieldSet i -> FieldSet i
  null :: i -> FieldSet i -> Bool
  singleton :: i -> Field i -> FieldSet i
  insert :: i -> Field i -> FieldSet i -> FieldSet i
  delete :: i -> Field i -> FieldSet i -> FieldSet i
  fieldSetToList :: i -> FieldSet i -> [Field i]
  fieldSetFromList :: i -> [Field i] -> FieldSet i

  joinFields :: i -> [Field i] -> Event i
  splitFields :: i -> Event i -> [Field i]
  channelLen :: i -> Field i -> Int

  closureStateInit :: i -> EventSet i -> ClosureState i
  closureStateNext :: i -> ClosureState i -> Field i -> ClosureState i
  closureRestore   :: i -> ClosureState i -> EventSet i
  viewClosureState :: i -> ClosureState i -> ClosureView
  viewClosureFields :: i -> ClosureState i -> FieldSet i
  seenPrefixInClosure :: i -> ClosureState i -> Bool

  prefixStateInit :: i -> Prefix i -> PrefixState i
  prefixStateNext :: i -> PrefixState i -> Field i -> Maybe (PrefixState i)
  prefixStateFinalize :: i -> PrefixState i -> Maybe (Prefix i)
  viewPrefixState :: i -> PrefixState i -> PrefixFieldView i

data ClosureView
  = InClosure
  | NotInClosure
  | MaybeInClosure
  deriving (Show,Eq,Ord)

data PrefixFieldView i
  = FieldOut (Field i)
  | FieldIn
  | FieldGuard (FieldSet i)
