----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Types
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Definitions of most of the types used in the interpreter.
-- Also Instance declarations for the core language type families.
-- 'INT' is the type (index) for the CSPM interpreter.
--
----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances,TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module CSPM.Interpreter.Types
where

import qualified Language.CSPM.AST as AST
import Language.CSPM.SrcLoc (SrcLoc)
import qualified CSPM.CoreLanguage as Core

import CSPM.Interpreter.SSet (SSet)

import Data.Digest.Pure.HashMD5 as HashMD5
import Data.IntMap as IntMap (IntMap,empty)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Reader
import Control.Applicative (Applicative(..))
import Control.Exception
import qualified Data.List as List
import Data.Typeable
import Data.Set (Set)
import Data.Map (Map)
import Data.Ord
import Data.Function

data INT
  deriving Typeable

type Event = [Field]
type instance Core.Event INT = Event
type instance Core.EventSet INT = ClosureSet
type instance Core.RenamingRelation INT = RenamingRelation
type instance Core.ClosureState INT = ClosureState
type Field = Value
type instance Core.Field INT = Field
type FieldSet = SSet Field
type instance Core.FieldSet INT = FieldSet
type Process = Core.Process INT
type instance Core.ExtProcess INT = SwitchedOffProc
type Digest = HashMD5.MD5Digest
type instance Core.Prefix INT = PrefixState
-- type instance CoreField.PrefixState INT = PrefixState
type instance Core.PrefixState INT = GenericBufferPrefix
type Sigma = ClosureSet

deriving instance Eq Process
deriving instance Ord Process
deriving instance Show Process

data ClosureSet
  = ClosureSet {
    closureSetTrie :: PrefixTrie
   ,closureSetDigest :: Digest
   } deriving (Show, Typeable)

instance Ord ClosureSet where
  compare = comparing closureSetDigest
instance Eq ClosureSet where
  (==) = on (==) closureSetDigest


data RenamingRelation
  = RenamingRelation {
    renamingPairs :: Set (Event,Event)
   ,renamingDomain :: Set Event
   ,renamingRange :: Set Event
   ,renamingDigest :: Digest
   } deriving (Show)

instance Ord RenamingRelation where
  compare = comparing renamingDigest
instance Eq RenamingRelation where
  (==) = on (==) renamingDigest

data ClosureState
  = ClosureStateNormal {
     origClosureSet :: ClosureSet
    ,currentPrefixTrie :: PrefixTrie
  }
  | ClosureStateFailed { origClosureSet :: ClosureSet }
  | ClosureStateSucc {
     origClosureSet :: ClosureSet
    ,currentPrefixTrie :: PrefixTrie
  }
  deriving (Show,Eq,Ord)

data SwitchedOffProc
  = SwitchedOffProc {
    switchedOffDigest :: Digest
   ,switchedOffExpr :: AST.LExp
   ,switchedOffProcess :: Process
   }

instance Ord SwitchedOffProc where
  compare = comparing switchedOffDigest
instance Eq SwitchedOffProc where
  (==) = on (==) switchedOffDigest
instance Show SwitchedOffProc where
  show f = "(SwitchedOff " ++ (show $ switchedOffDigest f) ++ ")"

data PrefixState = PrefixState {
   prefixEnv :: Env
  ,prefixFields :: [AST.LCommField]
  ,prefixBody :: AST.LExp
  ,prefixRHS :: Process
  ,prefixDigest :: Digest
  ,prefixPatternFailed :: Bool
--  ,prefixLastInputBuffer :: [Value]
--  ,prefixOutputBuffer :: [Value]
  }

instance Ord PrefixState where
  compare = comparing prefixDigest
instance Eq PrefixState where
  a == b = prefixDigest a == prefixDigest b
instance Show PrefixState where
  show f = "(PrefixState " ++ (show $ prefixDigest f) ++ ")"

data GenericBufferPrefix
  = GBOut [Value] PrefixState
  | GBInput PrefixState
  | GBInputGuard FieldSet PrefixState
  | GBInputGeneric [Value] PrefixState
  | GBFinished PrefixState
  deriving (Show,Eq,Ord)

type Bindings = IntMap Value
data Env = Env {
   argBindings :: Bindings -- todo : merge argBindings and letBindings
  ,letBindings :: Bindings 
  ,letDigests :: IntMap Digest
  } deriving Typeable

emptyEnvirionment :: Env
emptyEnvirionment = Env {
   argBindings = IntMap.empty
  ,letBindings = IntMap.empty
  ,letDigests = IntMap.empty
  }

newtype EM x = EM { unEM ::Reader Env x }
  deriving (Functor, Applicative, Monad, MonadReader Env)

getArgBindings :: Env -> Bindings
getArgBindings = argBindings

getLetBindings :: Env -> Bindings
getLetBindings = letBindings

setArgBindings :: Env -> Bindings -> Env
setArgBindings env b = env {argBindings=b}

setLetBindings :: Env -> Bindings -> Env
setLetBindings env b = env {letBindings=b}

getEnv :: EM Env
getEnv = Reader.ask

class Monad m => Eval m where
  evalM :: AST.LExp -> m Value

data Value =
   VInt  Integer
 | VBool Bool
 | VList [Value]
 | VTuple [Value]
 | VDotTuple [Value]
 | VSet (Set Value)
 | VClosure ClosureSet
 | VFun FunClosure
 | VProcess Process
 | VChannel Channel
 | VUnit
-- cspm-special features
 | VAllInts
 | VAllSequences (Set Value)
--  | VAllEvents
 | VConstructor Constructor
 | VDataType [Constructor]
 | VNameType [FieldSet]
 | VPartialApplied FunClosure [Value]
 deriving (Ord, Eq, Typeable)

data FunClosure = FunClosure {
   getFunCases :: [AST.FunCase]
  ,getFunEnv :: Env
  ,getFunArgNum :: Int
  ,getFunId  :: Digest
  }

instance Eq FunClosure where
  a == b = getFunId a == getFunId b
instance Ord FunClosure where
  compare a b = compare (getFunId a) (getFunId b)
instance Show FunClosure where
  show f = "(FunClosure " ++ (show $ getFunId f) ++ ")"

data Constructor = Constructor {
   constrId ::Int
  ,constrName :: String
  ,constrFields :: [FieldSet]
  } deriving (Show)

instance Eq Constructor where
  a == b = constrId a == constrId b
instance Ord Constructor where
  compare a b = compare (constrId a) (constrId b)

data Channel = Channel
  {
    chanId :: Int
   ,chanName :: String
   ,chanLen :: Int
   ,chanFields :: [FieldSet] -- these are the fields proper excluding the channel itself
  } deriving (Show,Eq,Ord)

isChannelField :: Field -> Bool
isChannelField (VChannel {} ) = True
isChannelField _ = False

getChannel :: Field -> Channel
getChannel (VChannel x) = x
getChannel _ = error "Eval.hs : getChannel on non-Channel"

instance Show Value where
  show v = case v of
    VInt  i -> "(VInt " ++ show i  ++ ")"
    VBool b -> "(VBool " ++ show b  ++ ")"
    VList l -> "(VList " ++ show l  ++ ")"
    VTuple l -> "(VTuple " ++ show l  ++ ")"
    VDotTuple l -> "(VDotTuple " ++ show l  ++ ")"
    VSet s -> "(VSet " ++ show s ++ ")"
    VClosure s -> "(VClosure " ++ show s ++ ")"
    VProcess p -> "(VProcess " ++ show p ++ ")"
    VChannel c -> "(VChannel " ++ show c ++ ")"
    VFun _ -> "(VFun Functionclosure)"
    VUnit -> "VUnit"
    VAllInts -> "VAllInts"
    VAllSequences _  -> "VAllSequences"
--  VAllEvents -> "VAllEvents"
    VConstructor c -> "(VConstructor " ++ (show $ constrName c) ++ ")"
    VDataType l
      -> "(VDataType " ++ (concat $ List.intersperse " " $ map (show . constrName) l ) ++")"
    VNameType _ -> "VNameType"
    VPartialApplied {} -> "(Partially applyed function)" 

data PrefixTrie
  = PTNil
  | PTAny PrefixTrie
  | PTMap (Map Value PrefixTrie)
  | PTRec (Set Value) PrefixTrie  --rectangular closuresets (e.g. channels)
--  | PTInt PrefixTrie -- any Int field : todo generarlise this
  | PTSingle Value PrefixTrie
  | PTClosure PrefixTrie
  deriving (Show,Eq,Ord)

data InterpreterError
  = ScriptError {errMsg :: String, errLoc :: Maybe SrcLoc, errVal :: Maybe Value}
  | FeatureNotImplemented {errMsg :: String, errLoc :: Maybe SrcLoc }
  | TypingError {errMsg :: String, errLoc :: Maybe SrcLoc, errVal :: Maybe Value}
  | InternalError {errMsg :: String, errLoc :: Maybe SrcLoc, errVal :: Maybe Value }
  | PatternMatchError {errMsg :: String ,errLoc :: Maybe SrcLoc}
  deriving (Show,Typeable)

throwScriptError :: String -> Maybe SrcLoc -> Maybe Value -> a
throwScriptError m l v = throw $ ScriptError m l v
throwFeatureNotImplemented :: String -> Maybe SrcLoc -> a
throwFeatureNotImplemented m l = throw $ FeatureNotImplemented m l
throwTypingError :: String -> Maybe SrcLoc -> Maybe Value -> a
throwTypingError m l v = throw $ TypingError m l v
throwInternalError :: String -> Maybe SrcLoc -> Maybe Value -> a
throwInternalError m l v = throw $ InternalError m l v
throwPatternMatchError :: String -> Maybe SrcLoc -> a
throwPatternMatchError m l = throw $ PatternMatchError m l

instance Exception InterpreterError
