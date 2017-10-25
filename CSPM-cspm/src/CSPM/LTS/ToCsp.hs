----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.LTS.ToCsp
-- Copyright   :  (c) Fontaine 2009 - 2011
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
--  dump a Lts as Csp-Specifications suitable for FDR-refinementcheck

module CSPM.LTS.ToCsp
  (
  ltsToCsp
  )
where

import CSPM.CoreLanguage hiding (Field)

import CSPM.Interpreter (INT) -- todo : remove this dependency
import CSPM.FiringRules.Rules
import CSPM.FiringRules.Verifier (viewRule)

import CSPM.Interpreter.Hash -- todo : remove this depnedency

import CSPM.LTS.LTS

import Text.PrettyPrint.HughesPJClass

import qualified Data.Map as Map
import Data.List as List

-- | Translate an LTS to a CSP specification suitable for reloading it with FDR.
ltsToCsp ::
     Process INT -- ^ the intial process
  -> LTS         -- ^ the LTS
  -> Doc

ltsToCsp process lts
  =      text "GPINIT = " <> procToCsp process
    $+$ (vcat $ map transToCsp $ Map.assocs lts)
  where
    procToCsp :: Process INT -> Doc
    procToCsp p = text "GP_" <> (text $ show $ hash p)

    transToCsp :: (LtsNode, [Rule INT]) -> Doc
    transToCsp (p, transitions)
         =      (procToCsp (nodeProcess p) <> text " = ")
            $+$ nest 4 (parens  $ case (List.partition isTauRule transitions) of
         ([],[]) | nodeProcess p == Omega -> text "SKIP"
         ([],[])              -> text "STOP"
         (tauRules,[]) -> tauTransList tauRules
         ([], nonTau) ->  eventTransList nonTau
         (tauRules,nonTau) -> vcat [
             parens $ eventTransList nonTau
            ,nest 4 $ text "[>"
            ,parens $ tauTransList tauRules
            ])

    tauTransList :: [Rule INT] -> Doc
    tauTransList = vcat . punctuate (text " |~| ") . map showTrans

    eventTransList :: [Rule INT] -> Doc
    eventTransList = vcat . punctuate (text " [] ") . map showTrans

    showTrans :: Rule INT -> Doc
    showTrans r = eventToCsp trans <> procToCsp to
       where
         (_from,trans,to) = viewRule r

    eventToCsp :: TTE INT -> Doc
    eventToCsp e = case e of
      TickEvent -> empty
      TauEvent -> empty
      SEvent l -> (hcat $ punctuate (text ".") $ List.map pPrint l) <> text "->"
