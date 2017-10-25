----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Prefix
-- Copyright   :  (c) Fontaine 2009 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
--
--
----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module CSPM.Interpreter.Prefix
(
  initPrefix
 ,viewPrefixState
 ,prefixStateNext
 ,prefixStateFinalize
)
where

import CSPM.Interpreter.Types as Types
import CSPM.Interpreter.PatternMatcher
import CSPM.Interpreter.Eval

import qualified CSPM.CoreLanguage as Core
import Language.CSPM.AST as AST hiding (Bindings)

import Data.List as List

initPrefix :: PrefixState -> PrefixState
initPrefix = id

viewNextPrefixField :: PrefixState -> CommField
viewNextPrefixField = unLabel . head . prefixFields

viewPrefixState :: PrefixState -> Core.PrefixFieldView INT
viewPrefixState p | List.null $ prefixFields p
  = throwScriptError "viewPrefixState: no fields" Nothing Nothing
viewPrefixState p = case viewNextPrefixField p of
  OutComm out -> Core.FieldOut $ runEM (evalOutField out) env
  InComm _pat -> Core.FieldIn
  InCommGuarded _pat g -> Core.FieldGuard $ runEM (evalFieldSet g) env
  where env = prefixEnv p

prefixStateNext :: PrefixState -> Field -> Maybe PrefixState
prefixStateNext p _field | List.null $ prefixFields p
  = throwScriptError "prefixStateNext no fields" Nothing Nothing
prefixStateNext p field = case viewNextPrefixField p of
{- todo ::
  we must check that the Field is OK here
  we should use the lookahead scheme of the GenericBufferPrefix
-}
  OutComm _out -> return $ p { prefixFields = tail $ prefixFields p }
  InComm pat -> prefixBindInput field pat
  InCommGuarded pat _g -> prefixBindInput field pat
  where
    env = prefixEnv p
    prefixBindInput f pat = do
      newBinds <- tryMatchStrict (argBindings env) pat f
      return p {
           prefixFields = tail $ prefixFields p
          ,prefixEnv = setArgBindings env newBinds }

prefixStateFinalize :: PrefixState -> Maybe PrefixState
prefixStateFinalize p | prefixPatternFailed p = Nothing
prefixStateFinalize p | not $ List.null $ prefixFields p
  = throwScriptError "prefixStateFinalize: unsynchronized fields left" Nothing Nothing
prefixStateFinalize p
  = Just $ p { prefixRHS = runEM (evalProcess $ prefixBody p) (prefixEnv p) }  
