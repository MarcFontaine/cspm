----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Pretty
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- A pretty-printer for values.
--
----------------------------------------------------------------------------
module CSPM.Interpreter.Pretty
where

import CSPM.Interpreter.Types
import CSPM.Interpreter.ClosureSet (closureToSet)

import Text.PrettyPrint.HughesPJClass
import qualified Data.Set as Set

instance Pretty Value where
  pPrint v = case v of
    VInt  i -> pPrint i
    VBool True -> text "true"
    VBool False -> text "false"
    VList l -> text "<" <> commaList l <> text ">"
    VTuple l -> parens $ commaList l
    VDotTuple l -> parens $ hcat $ punctuate (text ".") $ map pPrint l
    VSet s -> text "{" <> (commaList $ Set.toList s) <> text "}"
    VClosure s -> text "{|" <> (commaList $ Set.toList $ closureToSet s) <> text "|}"
    VProcess _ -> unPrintable "a process"
    VChannel c -> text $ chanName c
    VFun _ -> unPrintable "a function closure"
    VUnit -> text "()"
    VAllInts -> unPrintable "set of all Integer"
    VAllSequences _  -> unPrintable "VAllSequences"
    VConstructor c -> text $ constrName c
    VDataType _ -> text "datatype"
    VNameType _ -> text "nametype"
    VPartialApplied {} -> unPrintable "partially applyed function"
    _ -> unPrintable $ show v
    where
      commaList :: [Value] -> Doc
      commaList = hcat . punctuate comma . map pPrint
      unPrintable :: String -> Doc
      unPrintable s = parens $ text "{-" <> text s <> text "-}"