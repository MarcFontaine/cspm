----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Hash
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
-- 
-- Instances of the Hash class for interpreter types and core language types
--
----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CSPM.Interpreter.Hash
(
  mix
 ,mix3
 ,MD5Digest (..)
 ,Hash (..)
 ,hs
 ,closureDigest
 ,mixInt
)
where

import CSPM.CoreLanguage hiding (PrefixState, Event)

import qualified Language.CSPM.AST as AST
import CSPM.Interpreter.Types as Types

import Data.Digest.Pure.HashMD5
import Data.Digest.Pure.MD5

import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

hs :: String -> Digest
hs = hash

instance Hash Value where
  hash = hashValue

hashValue :: Value -> Digest
hashValue v = case v of
   VInt i -> if i == fromIntegral int
     then mixInt (hs "VInt") int
     else error $ "Hash.hs : integer out of bounds" ++ show i
     where int = fromIntegral i
   VBool True  -> hs "VBool True"
   VBool False -> hs "VBool False"
   VList l -> foldHash (hs "VList") l
   VTuple l -> foldHash (hs "VTuple") l
   VDotTuple l -> foldHash (hs "VDotTuple") l
   VSet s  -> foldHash (hs "VSet") $ Set.toAscList s
   VClosure c -> mix (hs "VClosure") $ hash c
   VFun f     -> mix (hs "VFun") $ hash f
   VProcess p -> hashProcess p
   VChannel c -> mixInt (hs "VChannel") $ chanId c
   VUnit -> hs "VUnit"
   VAllInts -> hs "VAllInts"
   VAllSequences s -> foldHash (hs "VAllSequences" ) $ Set.toAscList s
--   VAllEvents -> hs "VAllEvents"
   VConstructor c -> mix (hs "VConstructor") $ hash c
   VDataType d -> foldHash (hs "VDataType") d
   VNameType _d -> error "Hash : hash nametype " --foldHash (hs "VNameType") d
   VPartialApplied f l -> mix3 (hs "VPartialApplied") (hash f) (hash l)

-- todo :: cache the digests in the envrionments
instance Hash AST.LExp where
  hash expr = mixInt (hs "AST.LExp") $ AST.unNodeId $ AST.nodeId expr

closureDigest :: AST.LExp -> Env -> AST.FreeNames -> Digest
closureDigest expr env free = foldHash (hs "closureDigest") ( (hash expr) : binds)
  where
    binds = map lookupAndHash $ IntMap.elems free
   {- todo : remove distinction between LetBound and NotLetBound
      store precomputed CspCore.Digest in Bindings -}
    lookupAndHash :: AST.UniqueIdent -> Digest
    lookupAndHash ident
      = mixInt h i
      where
        !i = AST.uniqueIdentId ident
        !h = case AST.bindType ident of
                AST.NotLetBound -> case IntMap.lookup i (argBindings env) of
                    Just val -> hash val
                    Nothing -> err
                AST.LetBound    -> case IntMap.lookup i (letDigests env) of
                    Just d -> d
                    Nothing -> err

        err = error ( "Hash.hs Bindings lookup failure :" 
                        ++ "\n\n" ++ show expr
                        ++ "\n\n" ++ show free
                        ++ "\n\n" ++ show ident
                        ++ "\n\n" ++ (show $ argBindings env)
                        )


hashProcess :: Types.Process -> Digest
hashProcess proc = case proc of
  Prefix e -> mix (hs "Prefix") $ hash e
  ExternalChoice a b -> mix3 (hs "ExtChoice") (hash a) (hash b)
  InternalChoice a b -> mix3 (hs "InternalChoice") (hash a) (hash b)
  Interleave a b -> mix3 (hs "Interleave") (hash a) (hash b)
  Interrupt a b -> mix3 (hs "Interrupt") (hash a) (hash b)
  Timeout a b -> mix3 (hs "Timeout") (hash a) (hash b)
  Sharing a e b -> mix (hs "Sharing") $ mix3 (hash a) (hash e) (hash b)
  AParallel c1 c2 p q -> mix3 (hs "AParalle") (hash c1) $ mix3 (hash c2) (hash p) (hash q)
  Seq a b -> mix3 (hs "Seq") (hash a) (hash b)
  Hide s e -> mix3 (hs "Hide") (hash s) $ hash e
  Stop -> hs "Stop"
  Skip -> hs "Skip"
  Omega -> hs "Omega"
  AProcess i -> mixInt (hs "AProcess") i
  SwitchedOff p -> mix (hs "SwitchedOff") $ hash p
  RepAParallel l -> foldHash (hs "RepAParallel") l
  Renaming r p -> mix3 (hs "Renaming") (hash r) (hash p)
  Chaos c -> mix (hs "Chaos") $ hash c
  LinkParallel c p q -> mix (hs "LinkParallel") $ mix3 (hash c) (hash p) (hash q)
  Exception c p q -> mix (hs "Exception") $ mix3 (hash c) (hash p) (hash q)

instance Hash Types.Process where hash = hashProcess
instance Hash PrefixState where hash = prefixDigest
instance Hash SwitchedOffProc where hash = switchedOffDigest
instance Hash Types.ClosureSet where hash = closureSetDigest
instance Hash Types.RenamingRelation where hash = renamingDigest
instance Hash Constructor where hash c = mixInt (hs "Constructor") $ constrId c
instance Hash FunClosure where hash = getFunId

{-
hashEventSlice :: SSet Event -> Digest
hashEventSlice x = case x of
  Total -> hs "EventSlice Total"
  Empty -> hs "EventSlice Empty"
  (Proper x) -> foldHash (hs "EventSlice") $ Set.elems x
-}

instance Hash PrefixTrie where
  hash p = case p of
    PTNil -> hs "PTNil"
    PTAny l -> mix (hs "PTAny") $ hash l
    PTMap l -> foldHash (hs "PTMap") $ Map.assocs l
    PTRec s t -> mix (hs "PRRec") $ foldHash (hash t) $ Set.toList s
    PTClosure t -> mix (hs "PTClosure") $ hash t
    PTSingle v t -> mix3 (hs "PTSingle") (hash v) (hash t)