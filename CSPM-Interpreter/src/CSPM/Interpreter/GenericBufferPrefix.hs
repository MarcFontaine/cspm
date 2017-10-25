----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.GenricBufferPrefix
-- Copyright   :  (c) Fontaine 2009 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- A wrapper around CSPM.Interpreter.Prefix with support for generic buffers.
--
----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module CSPM.Interpreter.GenericBufferPrefix
(
  initPrefix
 ,viewPrefixState
 ,prefixStateNext
 ,prefixStateFinalize
)
where

import qualified CSPM.CoreLanguage as Core
import Language.CSPM.AST as AST hiding (Bindings)

import CSPM.Interpreter.Types as Types
import qualified CSPM.Interpreter.Prefix as BasePrefix
import CSPM.Interpreter.SSet as SSet

import Data.List as List
import Control.Monad

initPrefix :: PrefixState -> GenericBufferPrefix
initPrefix = lookAhead . BasePrefix.initPrefix

viewPrefixState :: GenericBufferPrefix -> Core.PrefixFieldView INT
viewPrefixState p = case p of
  GBOut (h:_) _ -> Core.FieldOut h
  GBOut [] _ -> error "GenericBuffer.hs : viewPrefixState : internal error : empty buffer"
  GBInput _ -> Core.FieldIn
  GBInputGuard g _ -> Core.FieldGuard g
  GBInputGeneric _ _ -> Core.FieldIn
  GBFinished _ -> error "GenericBuffer.hs : viewPrefixState : no fields left"
 
prefixStateNext :: GenericBufferPrefix -> Field -> Maybe GenericBufferPrefix
prefixStateNext gbPrefix field = case gbPrefix of
  GBOut [h] p -> do
    guard $ h == field
    liftM lookAhead $ BasePrefix.prefixStateNext p (error "GenericBufferDummyFields")
  GBOut (h:t) p -> do
    guard $ h == field
    return $ GBOut t p
  GBInput p -> liftM lookAhead $ BasePrefix.prefixStateNext p field
  GBInputGuard g p -> do
    guard $ field `SSet.member` g
    liftM lookAhead $ BasePrefix.prefixStateNext p field
  GBInputGeneric b p -> return $ GBInputGeneric (field:b) p
  GBFinished _ -> error "GenericBuffer.hs : prefixStateNext : no fields left"

prefixStateFinalize :: GenericBufferPrefix -> Maybe PrefixState
prefixStateFinalize gbPrefix = case gbPrefix of
  GBInputGeneric buffer p -> case buffer of
   [] -> error "GenericBuffer.hs : empty dot tuple"
   [v] -> BasePrefix.prefixStateNext p v >>= BasePrefix.prefixStateFinalize
   l -> BasePrefix.prefixStateNext p (VDotTuple $ reverse l)
      >>= BasePrefix.prefixStateFinalize
  GBFinished p -> BasePrefix.prefixStateFinalize p
  _ -> error "GenericBuffer.hs : stateError"

lookAhead :: PrefixState -> GenericBufferPrefix
lookAhead p | List.null $ prefixFields p = GBFinished p
lookAhead p | isLastInputField p = GBInputGeneric [] p
lookAhead p = case BasePrefix.viewPrefixState p of
  Core.FieldOut v -> case v of
    (VDotTuple l) -> GBOut (splitTuple l) p
    x -> GBOut [x] p
  Core.FieldIn -> GBInput p
  Core.FieldGuard g -> GBInputGuard g p

isLastInputField :: PrefixState -> Bool
isLastInputField (PrefixState {
  prefixFields =  [unLabel -> InComm (unLabel -> VarPat _ )] 
  }) = True
{- todo : fields that end with wildcard c?_ -> x -}
isLastInputField _ = False

splitTuple :: [Value] -> [Value]
splitTuple [] = []
splitTuple l@(h:t) = case h of
  VConstructor c | not $ List.null $ constrFields c
    -> VDotTuple (take len l) : splitTuple (drop len l)
         where len = 1 + (length $ constrFields c)
  _ -> h : splitTuple t
