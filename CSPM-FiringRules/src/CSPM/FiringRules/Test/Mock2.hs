-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.CoreLanguage.FiringRules.Mock2
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- A mock implementation of CSP-Processes.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts,TypeSynonymInstances #-}
module CSPM.FiringRules.Test.Mock2
where

import CSPM.CoreLanguage
import CSPM.CoreLanguage.Event as Event
import CSPM.CoreLanguage.Field as Field

import CSPM.FiringRules.Test.Gen
import CSPM.FiringRules.HelperClasses

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad

data M2
m2 :: M2
m2 = undefined

type ProcessM2 = Process M2
type ClosureViewM2 = ClosureView

instance EqOrd M2
instance CSP2 M2
instance CSP1 M2

type instance Field M2 = FieldM2
data FieldM2
  = Chan Int
  | Field Int
  deriving (Show,Eq,Ord)

type instance FieldSet M2 = FieldSetM2
newtype FieldSetM2 = FieldSet {unFieldSet :: (Set FieldM2)}
  deriving (Show,Eq,Ord)

type instance Event M2 = EventM2
newtype EventM2 = Event {unEvent :: [FieldM2]}
  deriving (Show,Eq,Ord)

type instance EventSet M2 = EventSetM2
newtype EventSetM2 = EventSet {unEventSet :: (Set EventM2)}
  deriving (Show,Eq,Ord)

type instance Prefix M2 = PrefixM2
newtype PrefixM2 = PrefixM2 {unPrefixM2 :: InternalPrefix}
  deriving (Show,Eq,Ord)

type instance ExtProcess M2 = ExtProcessM2
newtype ExtProcessM2 = ExtProcess (Process M2)
  deriving (Show,Eq,Ord)

type instance ClosureState M2 = ClosureStateM2
data ClosureStateM2 = ClosureState Int [Field M2] EventSetM2
  deriving (Show)

type instance PrefixState M2 = PrefixStateM2
data PrefixStateM2 = PrefixState Int [Field M2] InternalPrefix
  deriving (Show)

type instance RenamingRelation M2 = RenamingRelationM2
data RenamingRelationM2 = RenamingRelation {unRenamingRelation :: Set (EventM2, EventM2)}
  deriving (Show, Eq, Ord)

deriving instance Show (Process M2)
deriving instance Eq (Process M2)
deriving instance Ord (Process M2)


instance BL M2 where
  prefixNext (PrefixM2 c) event = if Event.member m2 event (allPrefixes c)
    then Just (error "M2 prefixNext p' undefined")
    else Nothing
  switchOn (ExtProcess x) = x

instance BE M2 where
  eventEq _ty a b = a == b
  member _ty a (EventSet s) = a `Set.member` s
  intersection _ty (EventSet a) (EventSet b) = EventSet $ a `Set.intersection` b
  difference _ty (EventSet a) (EventSet b) = EventSet $ a `Set.difference` b
  null _ty (EventSet a) = Set.null a
  singleton _ty a = EventSet $ Set.singleton a
  union = error "BE M2 union undefined (Mock2)"
  insert _ty a (EventSet s) = EventSet $ a `Set.insert` s
  delete _ty a (EventSet s) = EventSet $ a `Set.delete` s
  eventSetToList _ty (EventSet s) = Set.toList s
  allEvents _ty = EventSet $ Set.fromList allEventsList
  isInRenaming _ty (RenamingRelation r) e1 e2 = (e1,e2) `Set.member` r
  imageRenaming _ty (RenamingRelation r) e
    = Set.toList $ Set.map snd $ Set.filter ((==) e . fst) r
  preImageRenaming _ty (RenamingRelation r) e
    = Set.toList $ Set.map fst $ Set.filter ((==) e . snd) r
  isInRenamingDomain _ty e (RenamingRelation r)
    = not $ Set.null $ Set.filter ((==) e . fst) r
  isInRenamingRange _ty e (RenamingRelation r)
    = not $ Set.null $ Set.filter ((==) e . snd) r
  getRenamingDomain _ty = Set.toList . Set.map fst . unRenamingRelation
  getRenamingRange _ty = Set.toList . Set.map snd . unRenamingRelation
  renamingFromList _ty = RenamingRelation . Set.fromList
  renamingToList _ty = Set.toList . unRenamingRelation
  singleEventToClosureSet _ty = EventSet . Set.singleton


allEventsList :: [Event M2]
allEventsList = List.concat [
     Event <$> [[Chan 1]]
    ,Event <$> sequence [[Chan 2],map Field [1,2,3]]
    ,Event <$> sequence [[Chan 3],map Field [1,2,3],map Field [1,2,3]]
    ]

instance Arb M2 where
  genPrefix _ty = genPrefixM2
  arbitraryEvent _ty = elements $ allEventsList
  arbitraryEventSet _ty = elements $ map (EventSet . Set.fromList)
    $ List.subsequences $ allEventsList

genPrefixM2 :: Arbitrary ProcessM2 => EventM2-> Gen (Prefix M2)
genPrefixM2 event = liftM PrefixM2 (genInternalPrefix event)

data InternalPrefix = InternalPrefix {internalPrefixFields :: [MField]}
  deriving (Show,Eq,Ord)

data MField
  = MOut (Field M2)
  | MIn
  | MGuard (FieldSet M2)
  deriving (Show,Eq,Ord)

allPrefixes :: InternalPrefix -> EventSetM2
allPrefixes p
  = EventSet $ Set.fromList $ map (joinFields m2)
     $ mapM enumField $ internalPrefixFields p
  where
    enumField :: MField -> [FieldM2]
    enumField (MOut f) = [f]
    enumField MIn = map Field [1,2,3]
    enumField (MGuard f) = fieldSetToList m2 f

genInternalPrefix :: Event M2 -> Gen InternalPrefix
genInternalPrefix e = do
  f <- genMFields e
  return $ InternalPrefix f

{- generate fields that at least contain the given event -}
genMFields :: Event M2 -> Gen [MField]
genMFields (Event []) = error "Mock2.hs genMFields : empty event"
genMFields (Event (chan:rest)) = do
  l <- mapM genf rest
  return (MOut chan : l)
  where
    genf f = frequency [
       (10,return $ MOut f)
      ,(10,return MIn)
      ,(10,return $ MGuard $ FieldSet $ Set.singleton f) -- todo more interesting guards
      ]

instance BF M2 where
  fieldEq _ty a b = a == b
  member _ty e (FieldSet s) = e `Set.member` s
  intersection _ty (FieldSet a) (FieldSet b) = FieldSet $ a `Set.intersection` b
  difference _ty (FieldSet a) (FieldSet b) = FieldSet $ a `Set.difference` b
  null _ty (FieldSet a) = Set.null a
  singleton _ty a = FieldSet $ Set.singleton a
  union = error "BF M2 union undefined"
  insert _ty a (FieldSet s) = FieldSet $ a `Set.insert` s
  delete _ty a (FieldSet s) = FieldSet $ a `Set.delete` s
  fieldSetToList _ty (FieldSet s) = Set.toList s

  joinFields _ty = Event
  splitFields _ty (Event l) = l
  channelLen _ty (Chan i) = i
  channelLen _ty _ = error "MockM2 channelLen : not a channel field"

  closureStateInit _ty s = ClosureState 0 [] s

  closureStateNext _ty (ClosureState i l s) f = ClosureState (i+1) (l++[f]) s
  closureRestore   _ty (ClosureState _ _ s) = s

  viewClosureState _ty = viewClosureStateM2
  viewClosureFields _ty = viewClosureFieldsM2
  seenPrefixInClosure ty (ClosureState _i l s) = Event.member ty (Event l) s

  prefixStateInit _ty p = PrefixState 0 [] $ unPrefixM2 p
{-
  prefixStateNext _ty (PrefixState i l p) f = Just $ PrefixState (i+1) (l++[f]) p

this assumes that computeNext allways passes valid fields
(this is not the case, but why ?)

-}
  prefixStateNext _ty (PrefixState i l p) field
    = if checkField (internalPrefixFields p !!i) field
        then Just $ PrefixState (i+1) (l++[field]) p
        else Nothing
    where
      checkField :: MField -> Field M2 -> Bool
      checkField f v = case f of
        MOut v2 -> v == v2
        MIn -> True
        MGuard s -> Field.member m2 v s  

  prefixStateFinalize _ty (PrefixState _ _ p)
     = Just $ PrefixM2 p -- just return the original process
  viewPrefixState _ty  (PrefixState i _ (InternalPrefix l)) = case ( l !! i) of
    MOut val -> FieldOut val
    MIn -> FieldIn
    MGuard s -> FieldGuard s  
  fieldSetFromList _ty = FieldSet . Set.fromList


viewClosureStateM2 :: ClosureStateM2 -> ClosureViewM2
viewClosureStateM2 (ClosureState _i p s)
  = if List.null possiblePrefixes
      then NotInClosure
      else MaybeInClosure
  where 
    l :: [Event M2]
    l = eventSetToList m2 s
    possiblePrefixes :: [[Field M2]]
    possiblePrefixes = filter (List.isPrefixOf p) $ map (splitFields m2) l


viewClosureFieldsM2 :: ClosureStateM2 -> FieldSetM2
viewClosureFieldsM2 (ClosureState i p s)
  = FieldSet $ Set.fromList $ map head rests    
  where
    l :: [Event M2]
    l = eventSetToList m2 s
    possiblePrefixes :: [[Field M2]]
    possiblePrefixes = filter (List.isPrefixOf p) $ map (splitFields m2) l
    rests = map (drop i) possiblePrefixes
