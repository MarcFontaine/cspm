-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.Search
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Definition of the Search Monad.
-- Currently just a small wrapper around the tree-monad package
--
-----------------------------------------------------------------------------

module CSPM.FiringRules.Search
(
  Search
 ,runSearch
 ,anyOf
)
where

import qualified Control.Monad.SearchTree as MS
import Control.Parallel.TreeSearch (parSearch)
import Control.Monad

type Search a = MS.Search a

-- | Run the search monad.
runSearch :: Search a -> [a]
runSearch = parSearch . MS.searchTree

-- | Lift a list to the search monad.
anyOf :: [a] -> Search a
anyOf = msum . map return