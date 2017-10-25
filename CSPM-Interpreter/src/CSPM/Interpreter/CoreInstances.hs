----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.CoreInstances
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module defines some class instances that make the interpreter
-- an implementation of the interface defined in package CSPM-CoreLanguage.
--
----------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_Ghc -fno-warn-orphans #-}
module CSPM.Interpreter.CoreInstances
(
)
where

import CSPM.CoreLanguage as Core
import CSPM.CoreLanguage.Field
import CSPM.CoreLanguage.Event

import CSPM.Interpreter.Types as Types
import CSPM.Interpreter.ClosureSet as ClosureSet
import CSPM.Interpreter.Renaming as Renaming
import CSPM.Interpreter.GenericBufferPrefix as Prefix
import qualified CSPM.Interpreter.SSet as SSet

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List as List

deriving instance Show (Core.TTE INT)

noInstance :: String -> a
noInstance i
  = throwInternalError ("interpreter core-instances : no instance : " ++ i) Nothing Nothing

instance BE INT where
  eventEq _ty = (==)
  member  _ty event c = ClosureSet.memberPrefixTrie event $ closureSetTrie c
{-
these functions are only needed for the total-event-view
they are not needed for field-wise computation of events
todo: make a seperate class for these functions

  intersection  _ty = SSet.intersection
  difference  _ty = SSet.difference
  union  _ty = SSet.union
  null  _ty = SSet.null
  singleton _ty = SSet.singleton
  insert  _ty = SSet.insert
  delete  _ty = SSet.delete
  eventSetToList  _ty = SSet.toList
  allEvents = error "CoreInstances : BE :allEvents"
-}
  intersection  _ty = noInstance "BE : intersection"
  difference  _ty = noInstance "BE : difference"
  union  _ty = noInstance "BE : union"
  null  _ty = noInstance "BE : null "
  singleton _ty = noInstance "BE : singleton"
  insert  _ty = noInstance "BE : insert"
  delete  _ty = noInstance "BE : delete"
  eventSetToList _ty = map hackValueToEvent . Set.toList . closureToSet
  allEvents = noInstance "BE :allEvents"
  isInRenaming _ty = Renaming.isInRelation
  isInRenamingDomain _ty e rel = Set.member e $ renamingDomain rel
  isInRenamingRange _ty e rel = Set.member e $ renamingRange rel
  getRenamingDomain _ty = Set.toList . renamingDomain
  getRenamingRange _ty = Set.toList . renamingRange
  imageRenaming _ty = Renaming.imageRenaming
  preImageRenaming _ty = Renaming.preImageRenaming
  renamingToList = noInstance "BE : renamingToList"
  renamingFromList = noInstance "BE : renamingFromList"
  singleEventToClosureSet _ty = ClosureSet.singleEventToClosureSet

instance BF INT where
  fieldEq  _ty = (==)
  member  _ty = SSet.member
  intersection _ty = SSet.intersection
  difference  _ty = SSet.difference
  union _ty = SSet.union
  null _ty = SSet.null
  singleton  _ty = SSet.singleton
  insert _ty = SSet.insert
  delete _ty = SSet.delete
  fieldSetToList _ty = SSet.toList
  fieldSetFromList _ty = SSet.fromList

  joinFields  _ty = id
  splitFields  _ty = id
  channelLen  _ty (VChannel c) = chanLen c
  channelLen  _ty v = error $ "channelLen: Expecting Channel. Found : " ++ show v 

  closureStateInit _ty c
    = ClosureStateNormal {origClosureSet = c ,currentPrefixTrie = closureSetTrie c}

  closureStateNext _ty = ClosureSet.closureStateNext

  closureRestore _ty closure = origClosureSet closure

  viewClosureFields _ty c = case c of
    ClosureStateFailed {} -> SSet.Empty
    _ -> case currentPrefixTrie c of
      PTAny _ -> SSet.Total
      PTMap m -> SSet.Proper $ Map.keysSet m
      PTRec s _ -> SSet.Proper s
      PTSingle v _ -> SSet.singleton v
      PTNil -> error "viewClosureFields : PTNil not implemented"
      PTClosure {} -> error "viewClosureFields : PTClosure not implemented"
  viewClosureState _ty closure = case closure of
    ClosureStateNormal {} -> MaybeInClosure
    ClosureStateFailed {} -> NotInClosure
    ClosureStateSucc   {} -> InClosure
  seenPrefixInClosure _ty closure = case closure of
    ClosureStateNormal {} -> True
    ClosureStateFailed {} -> False
    ClosureStateSucc   {} -> True
  prefixStateInit _ty = Prefix.initPrefix
  prefixStateNext _ty = Prefix.prefixStateNext
  prefixStateFinalize  _ty = Prefix.prefixStateFinalize
  viewPrefixState  _ty = Prefix.viewPrefixState

{-
todo:
better modelling of prefix with multiple fields
(all fields == one event) level
-}
instance BL INT where
  prefixNext p _event = Just $ prefixRHS p -- todo maybe sanity check with _event
  switchOn = switchedOffProcess


instance ShowEvent Types.Event where
  showEvent l = concat $ intersperse "." $ List.map showValue l

showValue :: Value -> String
showValue f = case f of
  VChannel chan -> chanName chan
  VInt i -> show i
  VBool True -> "true"
  VBool False -> "false"
  VConstructor c -> constrName c
  VTuple l -> "(" ++ listBody l ++ ")"
  VSet l -> "{" ++ (listBody $ Set.toList l) ++ "}"
  VList l -> "<" ++ listBody l ++ ">"
  VDotTuple l -> (concat $ intersperse "." $ List.map showValue l)
  _ -> throwFeatureNotImplemented ("showValue : missing match : " ++ show f) Nothing
  where
    listBody = concat . intersperse "," . List.map showValue

instance ShowTTE (TTE INT) where
  showTTE t = case t of
    TickEvent -> "tick"
    TauEvent -> "tau"
    SEvent e -> showEvent e
