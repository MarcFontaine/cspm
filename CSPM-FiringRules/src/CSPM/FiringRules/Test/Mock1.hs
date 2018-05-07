-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.Test.Mock1
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

{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-} 

module CSPM.FiringRules.Test.Mock1
where

import CSPM.CoreLanguage
import CSPM.CoreLanguage.Event

import CSPM.FiringRules.Test.Gen
import CSPM.FiringRules.HelperClasses

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad

data M1

m1 :: M1
m1 = undefined

type instance Prefix M1 = PrefixM1 
data PrefixM1 = PrefixM1 (Map Int (Process M1))
  deriving (Show,Eq,Ord)

type instance ExtProcess M1 = Process M1

deriving instance Show (Process M1)
deriving instance Eq (Process M1)
deriving instance Ord (Process M1)

instance EqOrd M1
instance CSP1 M1


type instance Event M1 = Int
type instance EventSet M1 = Set Int
type instance RenamingRelation M1 = Set (Int,Int)

instance BE M1 where
  eventEq _ty = (==)
  member _ty = Set.member
  intersection _ty = Set.intersection
  difference _ty = Set.difference 
  union = error "BE M1 union undefined"
  null _ty = Set.null
  singleton _ty = Set.singleton
  insert _ty = Set.insert
  delete _ty = Set.delete
  eventSetToList _ty = Set.toList
  allEvents _ty = Set.fromList [1..5]
  isInRenaming _ty rel a b = (a,b) `Set.member` rel
  imageRenaming _ty rel e = Set.toList $ Set.map snd $ Set.filter ((==) e . fst) rel
  preImageRenaming _ty rel e = Set.toList $  Set.map fst $ Set.filter ((==) e . snd) rel
  isInRenamingDomain _ty e rel = any ((==) e . fst) $ Set.toList rel
  isInRenamingRange _ty e rel = any ((==) e . snd) $ Set.toList rel
  getRenamingDomain _ty = Set.toList . Set.map fst
  getRenamingRange _ty = Set.toList . Set.map snd
  renamingFromList _ty = Set.fromList
  renamingToList _ty = Set.toList
  singleEventToClosureSet = error "BE M1 singleEventToClosureSet undefined (Mock1)"


instance BL M1 where
  switchOn = id
  prefixNext (PrefixM1 m) e = Map.lookup e m

instance Arb M1 where
  genPrefix _ty = genPrefixM1
  arbitraryEvent _ty = elements $ eventSetToList m1 $ allEvents m1
  arbitraryEventSet _ty = arbitraryEventSetM1

arbitraryEventSetM1 :: Gen (EventSet M1)
arbitraryEventSetM1 = elements $ map Set.fromList
    $ List.subsequences $ eventSetToList m1 $ allEvents m1

genPrefixM1 :: Event M1 -> Gen (Prefix M1)
genPrefixM1 event = do
  proc <- arbitrary
  transCount <- elements [1..5]
  extraTransitions <- replicateM transCount
    ((,) <$> arbitraryEvent m1 <*> arbitrary)
  return $ PrefixM1 $ Map.fromList ((event,proc) : extraTransitions)
