----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.LTS.LTS
-- Copyright   :  (c) Fontaine 2009 - 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
{-# LANGUAGE BangPatterns, DeriveDataTypeable,FlexibleContexts,RankNTypes  #-}

module CSPM.LTS.LTS
where
import CSPM.CoreLanguage.Event
import CSPM.FiringRules.Rules
import CSPM.Interpreter as Interpreter
import CSPM.Interpreter.Hash

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Function (on)
import CSPM.FiringRules.Verifier (viewRule)

data LtsNode
  = LtsNode {
    nodeDigest :: ! Interpreter.Digest
   ,nodeProcess :: Interpreter.Process
   } deriving Typeable

mkLtsNode :: Interpreter.Process -> LtsNode
mkLtsNode p = LtsNode {
   nodeDigest = hash p
  ,nodeProcess = p }

instance Ord  LtsNode where compare = comparing nodeDigest
instance Eq   LtsNode where (==) = on (==) nodeDigest
instance Show LtsNode where
  show f = "(LTSNode " ++ (show $ nodeDigest f) ++ ")"

type LTS = Map LtsNode [Rule INT]

-- | Compute the hash value of an LTS. (Warning: This does not include entry process)
hashLTS :: LTS -> Interpreter.Digest
hashLTS = mix (hs "CSPM.LTS.LTS_salt0") . hash . map hashNode . Map.assocs
  where
    hashNode :: (LtsNode,[Rule INT]) -> Interpreter.Digest
    hashNode (node,transList)
      = mix3 (hs "transisition") (nodeDigest node) $ hash $ map hashTrans transList
    hashTrans :: Rule INT -> Interpreter.Digest
    hashTrans r = let (from,trans,to) = viewRule r
      in mix3 (hash from) (hashEvent trans) (hash to)
    hashEvent ::
      forall i. Hash (CSPM.CoreLanguage.Event.Event i) => TTE i -> Digest
    hashEvent e = case e of
      TickEvent -> hs "tickEvent"
      TauEvent  -> hs "tauEvent"
      SEvent l -> hash l
