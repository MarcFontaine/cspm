----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Bindings
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
----------------------------------------------------------------------------
module CSPM.Interpreter.Bindings
where

import CSPM.Interpreter.Types
import Language.CSPM.AST hiding (Bindings)

import Control.Exception
import Control.Monad.Reader
import Data.IntMap as IntMap
import Data.List as List

lookupIdent :: LIdent -> EM Value
lookupIdent i = do
  b <- getEnv
  let binds = case bindType $ unUIdent $ unLabel i of
        LetBound    -> getLetBindings b
        NotLetBound -> getArgBindings b
  case (IntMap.lookup (identId i) binds) of
    Just v -> return v
    Nothing -> throw $ InternalError ("Bindings lookup failure :" ++ show i) Nothing Nothing

bindIdent :: LIdent -> Value -> Bindings -> Bindings
bindIdent key value bind = IntMap.insert (identId key) value bind

emptyBindings :: Bindings
emptyBindings = IntMap.empty  

lookupAllChannels :: EM [Channel]
lookupAllChannels = do
  letBinds <- liftM getLetBindings getEnv
  return $ List.map getChannel $ List.filter isChannelField 
    $ IntMap.elems letBinds
