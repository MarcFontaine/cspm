----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.ClosureSets
-- Copyright   :  (c) Fontaine 2009
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Utility functions dealing with closure sets.
--
----------------------------------------------------------------------------
{-
todo: redo this
todo: add testcases
this is all much to complex
-}
module CSPM.Interpreter.ClosureSet
where

import CSPM.Interpreter.Types as Types
import CSPM.Interpreter.SSet as SSet
import CSPM.Interpreter.Hash as Hash

import Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord

memberPrefixTrie :: [Field] -> PrefixTrie -> Bool
memberPrefixTrie [] PTNil = True
memberPrefixTrie (_:_) PTNil = False
memberPrefixTrie _ (PTClosure _) = True
memberPrefixTrie [] t
  = throwInternalError ("memberPrefix : number of fields mismatch" ++ show t)
     Nothing Nothing
memberPrefixTrie (_h:r) (PTAny t) = memberPrefixTrie r t
memberPrefixTrie (h:r) (PTMap m) = case Map.lookup h m of
  Just t -> memberPrefixTrie r t
  Nothing -> False
memberPrefixTrie (h:r) (PTRec set t)
  = if h `Set.member` set then memberPrefixTrie r t else False
memberPrefixTrie (h:r) (PTSingle v t)
  = if h == v then memberPrefixTrie r t else False

prefixTrieNext :: PrefixTrie -> Field -> Maybe PrefixTrie
prefixTrieNext t field = case t of
  PTNil  -> throwInternalError
    ("prefixTrieNext number of fields mismatch PTNil" ++ show field) Nothing Nothing
  PTAny new -> Just new
  PTMap m -> case Map.lookup field m of
    Just new -> Just new
    Nothing -> Nothing
  PTRec s new -> if field `Set.member` s then Just new else Nothing
  PTSingle v new -> if field == v then Just new else Nothing

closureStateNext :: ClosureState -> Field -> ClosureState
closureStateNext closure field = case closure of
    ClosureStateFailed {} -> closure
    ClosureStateSucc   {}
     -> closure {currentPrefixTrie = fromJust ptNext }
    ClosureStateNormal {} -> case prefixTrieNext (currentPrefixTrie closure) field of
      Nothing -> ClosureStateFailed {origClosureSet = origClosureSet closure}
      Just (PTClosure p) -> ClosureStateSucc
        {currentPrefixTrie = p, origClosureSet = origClosureSet closure}
      Just pt -> closure { currentPrefixTrie = pt}
    where ptNext = prefixTrieNext (currentPrefixTrie closure) field


setToClosure :: Set Value -> ClosureSet
setToClosure = mkClosureSet . setToPrefixTrie

mkClosureSet :: PrefixTrie -> ClosureSet
mkClosureSet x
  = ClosureSet {
     closureSetTrie = x
    ,closureSetDigest = mix (hs "ClosureSet") $ hash x }

setToPrefixTrie :: Set Value -> PrefixTrie
setToPrefixTrie = worker . map fromTuple . Set.toList
  where
    fromTuple (VDotTuple l ) = l
    fromTuple x = [x] -- channel a  and [|{a}|] apears in one evans example

    worker :: [[Value]] -> PrefixTrie
    worker [] = throwInternalError "setToPrefixTrie worker []" Nothing Nothing
    worker [[]] = PTNil
    worker l = let
      sl = sortBy (comparing head) l
      grps :: [[[Value]]]
      grps = groupBy (\a b -> (head a) == (head b)) sl
      withkeys :: [(Value,PrefixTrie)]
      withkeys = map (\g -> (head $ head g, worker $ map tail g)) grps
      in PTMap $ Map.fromList withkeys

closureToSet :: ClosureSet -> Set Value
closureToSet = prefixTrieToSet . closureSetTrie


hackValueToEvent :: Value -> Event
hackValueToEvent (VDotTuple l ) = l
hackValueToEvent x = [x] -- channel a  and [|{a}|] apears in one evans example

{- todo : this is too lowlevel -}
prefixTrieToSet :: PrefixTrie -> Set Value
prefixTrieToSet trie
  = Set.fromList $ worker [] [] trie
  where
    worker :: [Value] -> [Value] -> PrefixTrie -> [Value]
    worker acc path t = case t of
      PTNil -> (VDotTuple $ reverse path) : acc
      PTAny {} -> throwFeatureNotImplemented "cannot enumerate PTAny (Set,Seq,INT)"
                    Nothing Nothing
      PTMap m -> foldl' (add path) acc $ Map.assocs m
      PTRec s r -> foldl' (add path) acc $ zip (Set.elems s) $ repeat r
      PTSingle v r -> worker acc (v:path) r
      PTClosure l -> worker acc path l
    add :: [Value] -> [Value] -> (Value,PrefixTrie) -> [Value]
    add path acc (val,t) = worker acc (val:path) t
{- {|a,b,c|} -}
mkEventClosure :: [Value] -> EM ClosureSet
mkEventClosure l = if List.null l
  then throwScriptError "mkEventClosure : empty ClosureSet" Nothing Nothing
  else return $ mkClosureSet $ ptUnions $ map valueToPT l

{-
convert the things inside a {| |} to a prefix trie
-}
valueToPT :: Value -> PrefixTrie
valueToPT v = case v of
  VChannel c -> fieldsToPT [v] $ (SSet.Total : chanFields c)
  VDotTuple [] -> throwScriptError "valueToPT : empty dot-tuple" Nothing Nothing
  VDotTuple l@(VChannel c : _) -> fieldsToPT l (SSet.Total : chanFields c)
  VDotTuple _ -> throwScriptError "valueToPT : dot-tuple does not start with a channel"
        Nothing $ Just v
  _ -> throwScriptError "valueToPT: cannot make a event-closure of value" 
        Nothing $ Just v

{- fieldsToPT is a kind of zip -}
fieldsToPT :: [Value] -> [FieldSet] -> PrefixTrie
fieldsToPT (v:vr) (f:fr) = if v `SSet.member` f
    then PTSingle v $ fieldsToPT vr fr
    else throwScriptError "fieldsToPT : value outside channel definition" Nothing (Just v)
fieldsToPT [] f
  = PTClosure $ foldr ( \(SSet.Proper s) pt -> PTRec s pt) PTNil f
fieldsToPT v []
  = throwScriptError "fieldsToPT : more fields than declared in channel definition"
      Nothing (Just $ VDotTuple v)


{-
todo :
more efficent, direkt implementation
without converting to intermediate sets
-}
ptUnions :: [PrefixTrie] -> PrefixTrie
ptUnions = setToPrefixTrie . Set.unions . map prefixTrieToSet


singleEventToClosureSet :: Event -> ClosureSet
singleEventToClosureSet e
  = mkClosureSet $ foldr PTSingle PTNil $ e
