----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Renaming
-- Copyright   :  (c) Fontaine 2009
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Utility functions dealing with renaming relations.
----------------------------------------------------------------------------
{-
naive implementation of a renaming relation.
todo: mode efficent implementation
-}
module CSPM.Interpreter.Renaming
where

import CSPM.Interpreter.Types as Types
import CSPM.Interpreter.Hash as Hash
import CSPM.Interpreter.ClosureSet

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

{- todo speedup using hashing -}
toRenaming :: [(Value,Value)]-> RenamingRelation
toRenaming s = RenamingRelation {
    renamingPairs = pairs
   ,renamingDigest = renameDigest pairs
   ,renamingDomain = Set.map fst pairs
   ,renamingRange = Set.map snd pairs
  } where
  pairs = Set.unions $ map pairToRel s
  pairToRel :: (Value,Value) -> Set (Event,Event)
  pairToRel (a,b) = Set.fromList $ do
   (VDotTuple p1) <- Set.toList $ prefixTrieToSet $ valueToPT a
   return (p1, newPrefix ++ drop plen p1)
   where
     plen = prefixLen a    
     newPrefix = valueToPrefix b

  valueToPrefix :: Value -> [Value]
  valueToPrefix v = case v of
    VChannel _ -> [v]
    VDotTuple l@(VChannel _ : _) -> l
    VDotTuple [] -> throwScriptError "toRenaming1 : empty dot-tuple" Nothing Nothing
    VDotTuple _ -> throwScriptError "toRenaming1 : dot-tuple does not start with a channel"
                     Nothing (Just v)
    _ -> throwScriptError "toRenaming1 : cannot make renaming"
           Nothing $ Just v

  prefixLen :: Value -> Int
  prefixLen v = case v of
    VChannel _ -> 1
    VDotTuple l@(VChannel _ : _) -> length l
    VDotTuple [] -> throwScriptError "toRenaming2 : empty dot-tuple" Nothing Nothing
    VDotTuple _ -> throwScriptError "toRenaming2 : dot-tuple does not start with a channel"
                     Nothing (Just v)
    _ -> throwScriptError "toRenaming2 : cannot make renaming"
           Nothing $ Just v

renameDigest :: Set (Event,Event) -> Digest
renameDigest pairs
  = mix3 (hs "RenamingRelation")
     (hash $ map fst $ Set.toList pairs)
     (hash $ map snd $ Set.toList pairs)

{-
inverseRenaming :: RenamingRelation -> RenamingRelation
inverseRenaming r
  = RenamingRelation {
    renamingPairs = pairs
   ,renamingDigest = renameDigest pairs
   ,renamingDomain = renamingRange r
   ,renamingRange = renamingDomain r } 
  where
    pairs = Set.map (\(a,b) -> (b,a)) $ renamingPairs r
-}

{- sets have actually no advantage because we convert to lists anyway -} 

imageRenaming :: RenamingRelation -> Event -> [Event]
imageRenaming relation prefix
  = List.map snd $ List.filter (\(x,_) -> x == prefix) $ Set.toList $ renamingPairs relation

preImageRenaming :: RenamingRelation -> Event -> [Event]
preImageRenaming relation prefix
  = List.map fst $ List.filter (\(_,x) -> x == prefix) $ Set.toList $ renamingPairs relation

isInRelation :: RenamingRelation -> Event -> Event -> Bool
isInRelation rel a b = (a,b) `Set.member` renamingPairs rel