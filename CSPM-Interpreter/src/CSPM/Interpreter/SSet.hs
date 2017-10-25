----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.SSet
-- Copyright   :  (c) Fontaine 2009
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- 
--
----------------------------------------------------------------------------

{-
probably obsolete

Sets extended with a symbolic representations for
Empty maps
Total maps
and the difference of a total map and an normal map.
do we need a fiths case PosNeg Set Set ?
this is a general datastructure that deserves its own package
think of this in terms of the corresponding boolsche expressions !
-}
module CSPM.Interpreter.SSet
where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

-- | 'SSet' (symbolic Set) a 'Set' which handels a empty set,a total set
-- | and the inverse of a set symbolically
data SSet a=
   Proper { fromProper :: (Set a)}
 | Empty
 | Total
 | Inverse  { fromInverse :: (Set a)}
 deriving (Eq,Ord)

instance (Show a) => Show (SSet a) where
  show (Proper m) = "(Set ::" ++ show  (Set.toList m) ++ ")"
  show Empty = "EmptySet"
  show Total = "TotalSet"
  show (Inverse _) = "InverseSet"

intersection :: Ord a => SSet a -> SSet a -> SSet a

intersection(Proper s1) (Proper s2)
  = let t = Set.intersection s1 s2 in 
     if (Set.null t) then Empty else Proper t
intersection (Proper _) Empty = Empty
intersection (Proper m1) Total = Proper m1
intersection Total (Proper m2) = Proper m2
intersection Total Total = Total
intersection Empty Empty = Empty
intersection Empty (Proper _) = Empty
intersection Total Empty = Empty
intersection Empty Total = Empty
intersection Empty  (Inverse _) = Empty
intersection (Inverse _) Empty= Empty
-- representation for diff not implemented
intersection (Inverse s1) (Proper s2) = Proper $ Set.difference s2 s1
intersection (Proper s1) (Inverse s2) = Proper $ Set.difference s1 s2
intersection a@(Inverse _) Total = a
intersection Total a@(Inverse _) = a
intersection (Inverse s1) (Inverse s2) = Inverse $ Set.union s1 s2

difference :: Ord a => SSet a -> SSet a -> SSet a
difference(Proper s1) (Proper s2)
  = let t = Set.difference s1 s2 in 
     if (Set.null t) then Empty else Proper t

difference a@(Proper _) Empty = a
difference (Proper _) Total = Empty
difference Total (Proper m2) = Inverse m2
difference Total Total = Empty
difference Empty Empty = Empty
difference Empty (Proper _) = Empty
difference Total Empty = Total
difference Empty Total = Empty
difference Empty  (Inverse _) = Empty
difference a@ (Inverse _) Empty= a
-- representation for diff not implemented
difference (Inverse a) (Proper b) = Inverse $ Set.union a b
difference (Proper a) (Inverse b) = Inverse $ Set.union a b
difference (Inverse _) Total = Empty
difference Total (Inverse s) = Proper s
difference (Inverse a) (Inverse b) = Inverse $ Set.union a b

member :: Ord a => a -> SSet a -> Bool
member x (Proper s) = Set.member x s
member _ Empty = False
member _ Total = True
member x (Inverse s) = not $ Set.member x s

union :: Ord a => SSet a -> SSet a -> SSet a

union(Proper s1) (Proper s2) = Proper $ Set.union s1 s2
union a@(Proper _) Empty = a
union (Proper _) Total = Total
union Total (Proper _) = Total
union Total Total = Total
union Empty Empty = Empty
union Empty a@(Proper _) = a
union Total Empty = Total
union Empty Total = Total

union Empty  a@(Inverse _) = a
union a@(Inverse _) Empty = a
-- representation for diff not implemented
union (Inverse i) (Proper s) = Inverse $ Set.difference i s
union (Proper s) (Inverse i) = Inverse $ Set.difference i s
union (Inverse _) Total = Total
union Total (Inverse _) = Total
union (Inverse s1) (Inverse s2) = Inverse $ Set.intersection s1 s2

fromList :: Ord a => [a] -> SSet a
fromList = Proper . Set.fromList

unions :: Ord a => [SSet a] -> SSet a
unions l = List.foldl' union Empty l 

singleton :: Ord a => a -> SSet a
singleton = Proper . Set.singleton 

toList :: SSet a -> [a]
toList m = case m of
  Proper s -> Set.toList s
  Empty -> []
  Total -> error "SSet.hs : toList Total"
  Inverse _ -> error "SSet.hs : toList Inverse"

null :: SSet a -> Bool
null Empty = True
null (Proper m)
  = if (Set.null m) then error "SSet.hs : isAllwayEmpty :: not symbolic"
       else False
null _ = False


insert :: Ord a => a -> SSet a -> SSet a
insert e sy = case sy of
  Proper s -> Proper $ Set.insert e s
  _ -> error "SSet.hs : todo: implement insert"

delete :: Ord a => a -> SSet a -> SSet a
delete e sy = case sy of
  Proper s -> Proper $ Set.delete e s
  _ -> error "SSet.hs : todo: implement delete"
