----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.LTS.ToDot
-- Copyright   :  (c) Fontaine 2009
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- dump a Lts as a Dot-file
-- todo : completely rewrite (use dot-library)
module CSPM.LTS.ToDot
(
  mkDotFile
)
where

import CSPM.CoreLanguage
import CSPM.FiringRules.Verifier
import CSPM.FiringRules.Rules

import CSPM.LTS.LTS

import CSPM.Interpreter (INT)

import Text.PrettyPrint.HughesPJClass

import System.IO
import Data.Map as Map
import Data.List as List

import Control.Monad

-- todo : this is all pure no need for IO
-- todo : use dot-libray

-- | Dump a LTS as a .dot-file.
mkDotFile :: FilePath -> LTS -> IO ()
mkDotFile filename lts = do
 file <- openFile filename WriteMode
 let dup = hPutStrLn file
 dup "digraph stateSpace {"
 dup "margin = \"0\""
-- dup "page = \"11.0,8.5\""
-- dup "size = \"11.0,8.5\""
 dup "rotate = \"0\""
 dup "ratio = \"fill\""
 forM_ (Map.assocs lts) $ \adj -> do
   dumpNode dup adj
 dup "}"
 hClose file


dumpNode ::
     (String -> IO ())
  -> (LtsNode, [Rule INT])
  -> IO ()
dumpNode dup (proc,rules) = do
  dup $ dotNode proc
  forM_ rules $ \r -> dup $ dotEdge proc r

dotNode :: LtsNode -> String
dotNode proc = (showProc proc) ++ mkAttrib [mkLabel (show proc),color]
 where color = mkColor "black"

dotEdge :: LtsNode -> Rule INT-> String
dotEdge from rule
  = (showProc from) ++ " -> " ++ (showProc $ mkLtsNode to) ++ mkAttrib [label,color]
  where
{- _from is not equal to from, because some processes meight be switched off -}
    (_from,trans,to) = viewRule rule
    label = mkLabel $ eventToCsp trans
    color = mkColor "black" 

showProc :: LtsNode -> String
showProc x = "N"++ (show $ nodeDigest x)

mkAttrib :: [String] -> String
mkAttrib a = " [ " ++ (concat $ intersperse "," a) ++ " ]"

mkLabel :: String -> String
mkLabel l = "label = " ++ show l

mkColor :: String -> String
mkColor c = "color = " ++c

eventToCsp :: TTE INT -> String
eventToCsp e = case e of
  TickEvent -> "Tick"
  TauEvent -> "Tau"
  SEvent l -> concat $ intersperse "." $ List.map prettyShow l
