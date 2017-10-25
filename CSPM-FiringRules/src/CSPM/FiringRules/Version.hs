-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.Version
-- Copyright   :  (c) Fontaine 2012
-- License     :  BSD3
--
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Reflect the version number of this library.
--
-----------------------------------------------------------------------------

module CSPM.FiringRules.Version
where
import Paths_CSPM_FiringRules
import Data.Version (Version)

-- | The version of the CSPM-FiringRules library.
firingRulesVersion :: Version
firingRulesVersion = version
