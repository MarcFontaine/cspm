----------------------------------------------------------------------------
-- |
-- Module      :  Scripting.LuaUtils
-- Copyright   :  (c) Fontaine 2009 - 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
--
-- Utility function for Lua Scripting
----------------------------------------------------------------------------
{-# Language DeriveDataTypeable #-}
{-# Language ViewPatterns, RecordWildCards #-}
{-# Language ScopedTypeVariables, RankNTypes, GADTs, KindSignatures #-}
{-# Language FlexibleInstances #-}

module Scripting.LuaUtils
where

import qualified Scripting.Lua as Lua

import Control.Monad
import Data.Dynamic
import Data.Typeable (typeOf)
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C
import Control.Exception

data ErrorFromLua = ErrorFromLua Int String
  deriving Typeable

instance Show ErrorFromLua where
  show (ErrorFromLua err msg) = "lua-error " ++ show err ++ " :" ++ msg

instance Exception ErrorFromLua

data Export = forall a. Lua.LuaImport a => Export {
   exportName :: String
  ,exportHelp :: String
  ,exportFun :: a
  }

registerHsFunctions :: Lua.LuaState -> [Export] -> IO ()
registerHsFunctions l exportList = forM_ exportList
  $ \(Export name _help fun) -> Lua.registerhsfunction l ("_cspm_"++name) fun

type LuaObject = Ptr ()

-- this is crap
-- either we return something of type a
-- or a Error
-- but we loose the type info
data LuaReturn a
  = LuaReturnOK a
  | LuaReturnError LuaError

instance Lua.StackValue a => Lua.StackValue (LuaReturn a)
  where
    push l v = case v of
      LuaReturnOK a -> Lua.push l a
      LuaReturnError err -> toLuaObject err >>= Lua.pushlightuserdata l
    peek = error "peek: cannot read back value of type LuaReturn"
    valuetype = error "peek: cannot read back value of type LuaReturn"

newtype LuaError = LuaError {unLuaError :: SomeException }
  deriving Typeable

instance Lua.StackValue a => Lua.StackValue (Maybe a)
  where
    push l v = case v of
      Just a -> Lua.push l a
      Nothing -> Lua.push l ()

    peek l n = do
      t <- fmap (fmap $ \() -> Nothing) $ Lua.peek l n
      case t of
        Nothing -> fmap (fmap Just) $ Lua.peek l n
        _ -> return t

    valuetype _ = error "Maybe value type"

newtype LuaArray x = LuaArray {unLuaArray :: [x]}

instance Lua.StackValue a => Lua.StackValue (LuaArray a)
  where
    push s (LuaArray l) = do
      Lua.createtable s (length l) 0
      forM_ (zip [1..] l) $ \(ix::Int,val) -> do
        Lua.push s val
        Lua.rawseti s (-2) ix

    peek = error "peek LuaArray"
    valuetype = error "valuetype LuaArray"

newtype AssocTable = AssocTable {unAssocTable :: [Assoc]}
data Assoc = forall a b. (Lua.StackValue a, Lua.StackValue b)
  => a :-> b

instance Lua.StackValue AssocTable where
    push l (AssocTable t) = do
      Lua.createtable l (length t) 0
      forM_ t $ \(key :-> val) -> do
        Lua.push l key
        Lua.push l val
        Lua.rawset l (-3)

    peek = error "peek AssocTable"
    valuetype = error "valuetype AssocTable"


toLuaObject :: Typeable a => a -> IO LuaObject
toLuaObject
  = fmap castStablePtrToPtr . newStablePtr . toDyn

fromLuaObject :: forall a. Typeable a => LuaObject -> IO a
fromLuaObject ptr = do
  v <- deRefStablePtr $ castPtrToStablePtr ptr
  case fromDynamic v of
    Just a -> return a
    Nothing -> error $ "fromLuaObject: typeError expected : "
                 ++ expect ++ " found : " ++ found
      where
        expect = show $ typeOf (undefined :: a)
        found = show $ dynTypeRep v

-- todo: we have memory leaks, if we create
-- stablepointer and then throw an exception!!
handleException :: forall a. Lua.StackValue a => IO a -> IO (LuaReturn a)
handleException x
  = fmap LuaReturnOK x `catches` allHandler
  where
    allHandler = [ Handler someExc ]

    someExc :: SomeException -> IO (LuaReturn a)
    someExc = return . LuaReturnError . LuaError


foreign import ccall "lua.h lua_pcall" c_lua_pcall :: Lua.LuaState -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "wrapper" mkDebug :: (Lua.LuaState -> IO CInt) -> IO (FunPtr Lua.LuaCFunction)

call_debug :: Lua.LuaState -> Int -> Maybe Int -> IO (Maybe ErrorFromLua)
call_debug = \l narg nres -> do
  base <- fmap (subtract narg) $ Lua.gettop l
  debugFun <- mkDebug haskell_traceback
  Lua.pushcfunction l debugFun
  Lua.insert l base
  status <- fmap fromIntegral $
    c_lua_pcall l (fromIntegral narg) (fromIntegral $ maybe Lua.multret id nres) (fromIntegral base)
  Lua.remove l base
  when (status /= 0) $ void $ Lua.gc l Lua.GCCOLLECT 0
  freeHaskellFunPtr debugFun
  if status == 0
    then return Nothing
    else do
    t <- Lua.ltype l (-1)
    if t == Lua.TSTRING
      then do
        (Just s) <- Lua.peek l 1
        Lua.pop l 1
        return $ Just $ ErrorFromLua status s
      else do
        Lua.pop l 1
        return $ Just $ ErrorFromLua status ("no error string (type: " ++ show t ++ ")")
  where
    haskell_traceback :: Lua.LuaState -> IO CInt
    haskell_traceback l = do
      isString <- Lua.isstring l 1
      if not isString then return 1
      else do
        Lua.getglobal l "debug"
        isTable <- Lua.istable l (-1)
        if not isTable then Lua.pop l 1 >> return 1
          else do
          Lua.getfield l (-1) "traceback"
          isFunction <- Lua.isfunction l (-1)
          if not isFunction then Lua.pop l 2 >> return 1
            else do
            Lua.pushvalue l 1
            Lua.pushinteger l 2
            void $ Lua.call l 2 1
            return 1
