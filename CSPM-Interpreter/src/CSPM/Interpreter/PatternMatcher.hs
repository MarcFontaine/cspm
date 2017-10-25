----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.PatternMatcher
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Execute the selectors of a compilied pattern with a Value.
--
----------------------------------------------------------------------------
{-
todo :
Compiling selectors to pattern meight be an over-kill.
maybe its simpler and faster to implement direct pattern-matching
-}
{-# LANGUAGE BangPatterns, ViewPatterns #-}
module CSPM.Interpreter.PatternMatcher
(
 match
,tryMatchStrict
,tryMatchLazy
,boundNames
)
where

import Language.CSPM.AST as AST hiding (Bindings)
import CSPM.Interpreter.Types as Types
import CSPM.Interpreter.Bindings

import Data.Maybe
import qualified Data.Set as Set
import Data.Array.IArray as Array
import qualified Data.List as List

failedMatch :: Maybe Value
failedMatch = Nothing

typeError :: String -> Value -> Maybe Value
typeError x v = throwTypingError ("error in pattern-match : "++ x) Nothing $ Just v

-- todo make match strict !BangPattern
match :: Value -> Selector -> Maybe Value
match x SelectThis = return x

match (VInt a) (IntSel b) = if a==b then return VUnit else failedMatch
match v@(VInt _) _ = typeError "unexpected Int" v
match v        (IntSel _) = typeError "expecting Int" v

match (VBool True)  TrueSel = return VUnit
match (VBool False) TrueSel = failedMatch
match v             TrueSel = typeError "expecting Bool" v

match (VBool True)  FalseSel = failedMatch
match (VBool False) FalseSel = return VUnit
match v             FalseSel = typeError "expecting Bool" v
match v@(VBool _) _ = typeError "unexpected Bool" v

match (VChannel ch)  (ConstrSel ident)
  = if AST.uniqueIdentId ident == chanId ch then return VUnit else failedMatch
match (VConstructor (Types.Constructor i _ _))  (ConstrSel ident)
  = if AST.uniqueIdentId ident == i then return VUnit else failedMatch
match v             (ConstrSel c) = typeError ("expecting constructor " ++ show c) v
--  | DotSel Int Int Selector

match (VSet s)      (SingleSetSel b)
  = case Set.toList s of
      [e] -> match e b
      _ -> failedMatch
match v             (SingleSetSel _) = typeError "expecting a set" v

match (VSet s)      EmptySetSel
  =  if Set.null s then return VUnit else failedMatch
match v             EmptySetSel    = typeError "expecting a set" v

-- todo : really test this
match val@(VList l) p = case p of
  ListIthSel i next -> match (l !! i) next
  ListLengthSel 0 _next
    -> if null l then return VUnit else failedMatch
  ListLengthSel len next
    -> if length l == len then match val next else failedMatch
  HeadSel next
    -> if null l then failedMatch else match (head l) next
  HeadNSel len next -> do
    e <- saveTake len l
    match (VList e) next
  PrefixSel offset len next -> do
    e <- saveDrop offset l >>= saveTake len
    match (VList e) next
  TailSel next 
    -> if not $ null l then match (VList $ tail l) next else failedMatch
  SliceSel offsetL 0 next -> do
    e <- saveDrop offsetL l
    match (VList e) next
  SliceSel offsetL offsetR next -> do
    e <- saveDrop offsetL l
    let len = length e
    if len >= offsetR
       then match (VList $ take (len-offsetR) e) next
       else failedMatch
  SuffixSel offset len next -> do
    let s = length l
    if s >= offset + len
       then match (VList $ take len $ drop (s - offset - len) l) next
       else failedMatch
  other -> typeError ("matchList : not excpecting a List :" ++ show other) val

match t@(VTuple b)  (TupleLengthSel len next)
  = if length b == len then match t next else typeError "tuple wrong arity" t
match v             (TupleLengthSel _ _) = typeError "expecting tuple" v

match (VTuple b)    (TupleIthSel i next) = match (b !! i) next
match v             (TupleIthSel _ _) = typeError "expecting tuple" v

match (VDotTuple l) (DotSel i next) = match (l !! i) next
match v             (DotSel _ _) = typeError "expecting dot-tuple" v

match v p
  = throwInternalError ("hit catch-all case of pattern-matcher :" ++ show (v,p))
      Nothing $ Just v


saveDrop :: Int -> [a] -> Maybe [a]
saveDrop 0 l     = Just l
saveDrop _ []    = Nothing
saveDrop n (_:r) = saveDrop (n-1) r

saveTake :: Int -> [a] -> Maybe [a]
saveTake 0 _ = Just []
saveTake _ [] = Nothing
saveTake n (h:t) = do
  l <- saveTake (n-1) t
  Just $ h:l

{-
If we force the result we first force the value we match against
and then we check all selectors !
We must be careful about lazyness/strictness here !
todo: maybe use ST-Transformer to fold over the array / do some optimisations
avoid detour via lists

-}

-- | tryMatchStrict returns Nothing or a new Binding
tryMatchStrict :: Bindings -> LPattern -> Value -> Maybe Bindings
tryMatchStrict !binds p !val = case unLabel p of
  VarPat ident -> Just $ bindIdent ident val binds

  Selector sel ident -> case match val sel of
     Nothing -> Nothing
     Just valPart -> case ident of
       Nothing -> Just binds
       Just i -> Just $ bindIdent i valPart binds

  Selectors selectorL identArray -> do
    values <- matchGroup val selectorL
    let 
      addBind b i = case identArray Array.! i of 
        Just n -> bindIdent n (values Array.! i) b
        Nothing -> b
    return $ List.foldl' addBind binds $ Array.indices identArray
  _ -> throwInternalError "PatternMatcher : unsupported Pattern in strict match"
         (Just $ srcLoc p) Nothing

-- | tryMatchLazy allways return a new Binding, but may throw a error when
-- | any value in the binding is forced 
-- | forcing one of the values causes all the selectors being tested

{-
todo : Fix THISBUG:
If we have Selectors which all do not bind a new Ident,
still should to force the value , so that we can detect a failing match
-}

tryMatchLazy :: Bindings -> LPattern -> Value -> Bindings
tryMatchLazy binds (unLabel -> VarPat ident) val
  = bindIdent ident val binds
tryMatchLazy binds p@(unLabel -> Selector sel ident) val
  = case ident of
      Just i -> bindIdent i valPart binds
      Nothing -> binds -- THISBUG
  where
    valPart = case match val sel of
      Just v -> v
      Nothing -> throwPatternMatchError "pattern-match failure" (Just $ srcLoc p) $ Just val

tryMatchLazy binds sel@(unLabel -> Selectors selectorss identArray) val
  = List.foldl' addBind binds $ Array.indices identArray
  where
    values = case matchGroup val selectorss of
      Just x -> x
      Nothing -> throwPatternMatchError "pattern-match failure"
         (Just $ srcLoc sel) $ Just val
    addBind b i = case identArray Array.! i of 
      Just n -> bindIdent n (values Array.! i) b
      Nothing -> b -- THISBUG
tryMatchLazy _ p v
  = throwInternalError "PatternMatcher : unsupported Pattern in lazyMatch"
      (Just $ srcLoc p) $ Just v

{-
If we force one of the values, we also have to force all
of the corresponding linear selectors !!
todo : for efficiency specialize this for small selectors
-}
matchGroup :: Value -> Array Int Selector -> Maybe (Array Int Value)
matchGroup val sel = do
  l <- mapM (match val) $ Array.elems sel
  return $ Array.listArray (Array.bounds sel) l


boundNames :: LPattern -> [LIdent]
boundNames pat = case unLabel pat of
  VarPat i -> [i]
  Selector _ Nothing -> []
  Selector _ (Just i) -> [i]
  x@(Selectors {}) -> catMaybes $ Array.elems $ idents x
  _ -> throwInternalError "PatternMatcher : unexpected Pattern"
         (Just $ srcLoc pat) $ Nothing