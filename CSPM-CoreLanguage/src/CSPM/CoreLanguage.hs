-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.CoreLanguage
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Just re-exports.
--
-----------------------------------------------------------------------------
module CSPM.CoreLanguage
(
  module CSPM.CoreLanguage.Process
 ,module CSPM.CoreLanguage.ProcessWrapper
 ,module CSPM.CoreLanguage.Event
 ,module CSPM.CoreLanguage.Field
 ,coreLanguageVersion
)
where

import CSPM.CoreLanguage.Process
import CSPM.CoreLanguage.ProcessWrapper
import CSPM.CoreLanguage.Event hiding (BE (..))
import CSPM.CoreLanguage.Event (BE)
import CSPM.CoreLanguage.Field hiding (BF (..))
import CSPM.CoreLanguage.Field (BF)
import Paths_CSPM_CoreLanguage (version)
import Data.Version

-- | The version of the CSPM-CoreLangugage library
coreLanguageVersion :: Version
coreLanguageVersion = version