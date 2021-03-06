----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.Eval
-- Copyright   :  (c) Fontaine 2009 - 2011
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The main eval function of the Interpreter.
--
----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module CSPM.Interpreter.Eval
(
  eval
 ,runEM
 ,getSigma
 ,evalBool
 ,evalOutField
 ,evalFieldSet
 ,evalProcess
 ,evalModule
)
where

import qualified CSPM.CoreLanguage as Core

import Language.CSPM.AST as AST hiding (Bindings)

import CSPM.Interpreter.Types as Types
import CSPM.Interpreter.Bindings as Bindings
import CSPM.Interpreter.PatternMatcher
import CSPM.Interpreter.Hash as Hash
import CSPM.Interpreter.SSet as SSet
import CSPM.Interpreter.ClosureSet as ClosureSet
import CSPM.Interpreter.Renaming as Renaming

import Data.Digest.Pure.HashMD5 as HashClass

import Control.Arrow
import Control.Monad.Reader as Reader
import Control.Monad.State.Strict
--import Control.Monad hiding (guard)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.List as List

-- | Evaluate an expression in an envirionment.
runEval :: Env -> AST.LExp -> Value
runEval env expr = runEM (eval expr) env

-- | Run the 'EM' monad with a given envirionment.
runEM  :: EM x -> Env -> x
runEM action env = Reader.runReader (unEM action) env

runEnv :: Env -> EM x -> x
runEnv env action = Reader.runReader (unEM action) env

-- | Evaluate an expression in the 'EM' monad.
eval :: LExp -> EM Value
eval expr = case unLabel expr of
  Var v -> lookupIdent v
  IntExp i -> return $ VInt i
  SetExp (unLabel -> RangeOpen _ ) _
    -> throwFeatureNotImplemented "open sets" $ Just $ srcLoc expr
  SetExp r Nothing -> evalRange r >>= return . VSet . Set.fromList
  SetExp r (Just comps) -> do
    l <- evalSetComp ret comps
    return $ VSet l
    where ret = evalRange r >>= return . Set.fromList
  ListExp r Nothing -> liftM VList $ evalRange r
  ListExp r (Just comps) -> liftM VList $ evalListComp (evalRange r) comps
  ClosureComprehension (el, comps) -> do
    l <- evalListComp (mapM eval el) comps
    ClosureSet.mkEventClosure l >>= return . VClosure
  LetI decls freenames e -> do
    env <- getEnv
    let digest = closureDigest expr env freenames
    return $ runEval (processDeclList digest env decls) e
  Ifte cond t e -> do
    c <- evalBool cond
    if c then eval t else eval e
  CallFunction fkt args -> do
    f <- eval fkt
    parameter <- mapM eval $ concat args
    functionCall f parameter
  CallBuiltIn bi [[e]] -> builtIn1 bi e
  CallBuiltIn bi [[a,b]] -> builtIn2 bi a b
  CallBuiltIn _ _
    -> throwScriptError "calling builtIn with worng number of args"
         (Just $ srcLoc expr) Nothing
  Lambda {} -> throwInternalError "not expection Constructor Lambda"
                 (Just $ srcLoc expr) $ Nothing
  LambdaI freeNames patL body -> do
    env <- getEnv
    return $ VFun $ FunClosure {
       getFunCases = [FunCaseI patL body]
      ,getFunEnv = env
      ,getFunArgNum = length patL
      ,getFunId = closureDigest expr env freeNames
      }
  Stop -> return  $ VProcess $ Core.stop
  Skip -> return  $ VProcess $ Core.skip
  CTrue  -> return $ VBool True
  Events -> liftM VClosure evalAllEvents
  CFalse -> return $ VBool False
  BoolSet -> return $ VSet $ Set.fromList [VBool True,VBool False]
{-
  Many prob test contain unboundet INT
  IntSet -> return $ VAllInts
-}
  IntSet -> return $ VSet $ Set.fromList $ map VInt [0..100] --ToDo: Fix this !!
  TupleExp l -> mapM eval l >>= return . VTuple
  Parens e -> eval e
  AndExp a b -> do
    av <- evalBool a
    if av then eval b else return $ VBool False
  OrExp a b -> do
    av <- evalBool a
    if av then return $ VBool True else eval b
  NotExp e -> evalBool e >>= return . VBool . not
  NegExp e -> evalInt e >>= return . VInt . negate
  Fun1 bi e -> builtIn1 bi e
  Fun2 bi a b -> builtIn2 bi a b
  DotTuple l -> mapM eval l >>= return . VDotTuple . concatMap flatTuple
    where
      flatTuple (VDotTuple x ) = x
      flatTuple x = [x]
  Closure l -> mapM eval l >>= ClosureSet.mkEventClosure >>= return . VClosure
  ProcSharing s a b
   -> liftM3 Core.sharing
       (switchedOffProc a)
       (evalClosureExp s)
       (switchedOffProc b)
      >>= return . VProcess
  ProcAParallel aLeft aRight pLeft pRight
    -> liftM4 Core.aparallel
        (evalClosureExp aLeft)
        (evalClosureExp aRight)
        (switchedOffProc pLeft)
        (switchedOffProc pRight)
      >>= return . VProcess
  ProcLinkParallel l p q
    -> liftM3 Core.linkParallel
        (evalLinkList l)
        (switchedOffProc p)
        (switchedOffProc q)
       >>= return . VProcess
  ProcRenaming rlist gen proc -> do
    pairs <- case gen of
      Nothing -> mapM evalRenaming rlist
      Just gens -> evalListComp (mapM evalRenaming rlist ) $ unLabel gens
    p <- switchedOffProc proc
    return $ VProcess $ Core.renaming (toRenaming pairs) p
    where
      evalRenaming :: LRename -> EM (Value,Value)
      evalRenaming (unLabel -> Rename a b) = liftM2 (,) (eval a) (eval b)
  ProcRepSequence comp p
    -> evalProcCompL p comp >>= return . VProcess . Core.repSeq
  ProcRepInternalChoice comp p
    -> evalProcCompS p comp >>= return . VProcess . Core.repInternalChoice
  ProcRepExternalChoice comp p
    -> evalProcCompS p comp >>= return . VProcess . Core.repExternalChoice
  ProcRepInterleave comp p
    -> evalProcCompS p comp >>= return . VProcess . Core.repInterleave
  ProcRepAParallel comp c p
    -> evalListComp ret (unLabel comp) 
         >>= return . VProcess . Core.repAParallel
    where ret = do { x <- evalClosureExp c; y <- switchedOffProc p; return [(x,y)]}
  ProcRepLinkParallel comp link p
    -> liftM2 Core.repLinkParallel
        (evalLinkList link)
        (evalProcCompL p comp)
       >>= return . VProcess
  ProcRepSharing comp closure p -> do
    l <- evalProcCompS p comp
    c <- evalClosureExp closure
    return $ VProcess $ Core.repSharing c l
  PrefixI free chan fields body -> do
    env <- getEnv 
    return $ VProcess $ Core.prefix $ PrefixState {
        prefixEnv = env
       ,prefixFields = chanOut:fields
       ,prefixBody = body
       ,prefixRHS = throwInternalError "prefixRHS undefiend" (Just $ srcLoc expr) Nothing
       ,prefixDigest = closureDigest body env free
       ,prefixPatternFailed = False
     }
      where chanOut = setNode chan $ OutComm chan
  ExprWithFreeNames {}
    -> throwInternalError "didn't expect ExprWithFreeNames" (Just $ srcLoc expr) Nothing
  _ -> throwFeatureNotImplemented "hit catch-all case of eval function"
         $ Just $ srcLoc expr 

evalRange :: LRange -> EM [Value]
evalRange r = case unLabel r of
  RangeEnum l -> mapM eval l
  RangeClosed start end -> do
    s <- evalInt start
    e <- evalInt end
    return $ map VInt [s..e]
  RangeOpen start -> do
    s <- evalInt start
    return $ map VInt [s..]
  
evalBool :: LExp -> EM Bool
evalBool e = do
  v <- eval e
  case v of
    VBool b -> return b
    _  -> throwTypingError "expecting type Bool" (Just $ srcLoc e) $ Just v


evalInt :: LExp -> EM Integer
evalInt e = do
  v <- eval e
  case v of
    VInt b -> return b
    _ -> throwTypingError "expecting type Integer" (Just $ srcLoc e) $ Just v

evalList :: LExp -> EM [Value]
evalList e = do
  v <- eval e
  case v of
    VList l -> return l

--  used in mydemos/SimpleRepAlphParallel.csp SYSTEM
    VDataType l -> return $ map VConstructor l

--  because of a hack in RepAParalle
    VSet l -> return $ Set.toList l
--  because of a hack in evalProcCompS
    VClosure c -> return $ Set.toList $ closureToSet c

    _ -> throwTypingError "expecting type List" (Just $ srcLoc e) $ Just v

setFromValue :: Value -> EM (Set Value)
setFromValue v = case setFromValueM v of
  Just l -> return l
  Nothing -> throwTypingError "expecting type Set" Nothing $ Just v

evalSet :: LExp -> EM (Set Value)
evalSet e = do
  v <- eval e
  case setFromValueM v of
    Just l -> return l
    Nothing -> throwTypingError "expecting type Set" (Just $ srcLoc e) $ Just v

setFromValueM :: Value -> Maybe (Set Value)
setFromValueM v = case v of
  VSet l -> Just l
  VClosure c -> Just $ closureToSet c
  VDataType l -> Just $ Set.fromList  --used in basin_olderog_bank.csp
                     $ map VConstructor l
  _ -> Nothing

evalProcess :: LExp -> EM Process
evalProcess e = do
  v <- eval e
  case v of
    VProcess p -> return p
    _  -> throwTypingError "expecting type Process" (Just $ srcLoc e) $ Just v

evalClosureExp :: LExp -> EM ClosureSet
evalClosureExp e = do
  v <- eval e
  case v of
    VClosure x -> return x
--    VAllEvents -> evalAllEvents
    VSet s -> return $ setToClosure s
    _ -> throwTypingError "expecting type Event-Closure" (Just $ srcLoc e) $ Just v

listFromValue :: Value -> EM [Value]
listFromValue (VList l) = return l
listFromValue v = throwTypingError "expecting type List" Nothing $ Just v

builtIn1 :: LBuiltIn -> LExp -> EM Value
builtIn1 op expr 
  = case lBuiltInToConst op of
    F_Seq -> evalSet expr >>= return . VAllSequences
    F_card -> do
      s <- evalSet expr
      return $ VInt $ fromIntegral $ Set.size s
    F_empty  -> evalSet expr >>= return . VBool . Set.null
    F_head   -> do
      l <- evalList expr
      case l of
        [] -> throwScriptError "head of empty list" (Just $ srcLoc expr) Nothing
        h:_tail -> return h
    F_tail   -> do
      l <- evalList expr
      case l of
        [] -> throwScriptError "tail of empty list" (Just $ srcLoc expr) Nothing
        _head:rest -> return $ VList rest
    F_length -> evalList expr >>= return . VInt . fromIntegral . List.length
    F_Len2   -> evalList expr >>= return . VInt . fromIntegral . List.length
    F_Union -> do
      s <- evalSet expr 
      setList <- mapM setFromValue $ Set.elems s
      return $ VSet $ Set.unions setList
    F_Inter  -> do
      s <- evalSet expr 
      setList <- mapM setFromValue $ Set.elems s
      case setList of
        [] ->  throwScriptError "intersection of empty set of sets" 
                  (Just $ srcLoc expr) Nothing
        l  -> return $ VSet $ List.foldl1' Set.intersection l
    F_set    -> evalList expr >>= return . VSet . Set.fromList
    F_Set    -> do
      s <- evalSet expr
      return $ VSet $ Set.fromList $ map (VSet . Set.fromList ) 
        $ List.subsequences $ Set.toList s
    F_concat -> do
      l <- evalList expr >>= mapM listFromValue
      return $ VList $ List.concat l
    F_null -> do
      l <- evalList expr
      return $ VBool (List.null l)
    F_CHAOS -> liftM (VProcess . Core.chaos) $ evalClosureExp expr
    _ -> throwInternalError "malformed AST1" (Just $ srcLoc expr) Nothing

builtIn2 :: LBuiltIn -> LExp -> LExp -> EM Value
builtIn2 op a b =
  case lBuiltInToConst op of
    F_union  -> setOp Set.union
    F_inter  -> setOp Set.intersection
    F_diff   -> setOp Set.difference
    F_member -> do
      av <- eval a
      s <- evalSet b
      return $ VBool $ Set.member av s
    F_Seq    -> throwFeatureNotImplemented "builtIn2 FSeq" Nothing
    F_elem   -> do
      av <- eval a
      l  <- evalList b
      return $ VBool $ List.elem av l
    F_Concat -> do
      x <- evalList a
      y <- evalList b
      return $ VList $ x ++y
    F_Mult   -> intOp (*)
    F_Div    -> intOp div
    F_Mod    -> intOp mod
    F_Add    -> intOp (+)
    F_Sub    -> intOp (-)
    F_Eq     -> do
      x <- eval a
      y <- eval b
      return $ VBool (x == y)
    F_NEq    -> do
      x <- eval a
      y <- eval b
      return $ VBool (x /= y)
    F_GE     -> intCmp (>=)
    F_LE     -> intCmp (<=)
    F_LT     -> intCmp (<)
    F_GT     -> intCmp (>)
    F_Sequential -> procOp Core.seq
    F_Interrupt  -> procOp Core.interrupt
    F_ExtChoice  -> do
      x <- switchedOffProc a
      y <- switchedOffProc b
      return $ VProcess $ Core.externalChoice x y
    F_Timeout    -> procOp Core.timeout
    F_IntChoice  -> do
      x <- switchedOffProc a
      y <- switchedOffProc b
      return $ VProcess $ Core.internalChoice x y
    F_Interleave -> do
      x <- switchedOffProc a
      y <- switchedOffProc b
      return $ VProcess $ Core.interleave x y
    F_Hiding -> do
      proc <- switchedOffProc a
      hidden <- evalClosureExp b
      return $ VProcess $ Core.hide hidden proc
    F_Guard -> do
      cond <- evalBool a
      if cond then liftM VProcess $ switchedOffProc b
              else return $ VProcess Core.stop
    _ -> throwInternalError "malformed AST2"  (Just $ srcLoc op) Nothing
  where
    intOp :: (Integer -> Integer -> Integer) -> EM Value
    intOp o = do
      x <- evalInt a
      y <- evalInt b
      return $ VInt $ o x y
    intCmp :: (Integer -> Integer -> Bool) -> EM Value
    intCmp rel = do
      x <- evalInt a
      y <- evalInt b
      return $ VBool $ rel x y
    setOp :: (Set Value -> Set Value -> Set Value) -> EM Value
    setOp o = do
      x <- evalSet a
      y <- evalSet b
      return $ VSet $ o x y
    procOp :: (Process -> Process -> Process) -> EM Value
    procOp o = do
      x <- switchedOffProc a
      y <- switchedOffProc b
      return $ VProcess $ o x y

-- | Process a module and return the top-level envirionment.
evalModule :: Module INT -> Env
evalModule m
  = processDeclList (hs "TopLevelEnvirionment") emptyEnvirionment
      $ AST.moduleDecls m

type DeclM x = ReaderT (Digest,Env) (State (Bindings, IntMap Digest)) x

processDeclList :: Digest -> Env -> [LDecl] -> Env
processDeclList digest oldEnv decls =
  let
    (newBinds,newDigests)
       = execState action' (getLetBindings oldEnv, letDigests oldEnv)
    action :: DeclM ()
    action  = mapM_ processDecl decls
    action' = runReaderT action (digest,newEnv)
    newEnv  = oldEnv { letBindings = newBinds, letDigests = newDigests}
  in newEnv

bindIdentM :: LIdent -> Value -> DeclM ()
bindIdentM i v = do
  d <- asks fst
  modify $ \(values,digests) ->
    (bindIdent i v values
    ,IntMap.insert (identId i) (HashClass.mixInt d $ identId i) digests)

processDecl :: LDecl -> DeclM ()
processDecl decl = do 
  case unLabel decl of
    PatBind pat expr -> do
      finalEnv <- asks snd
      let rhs = runEval finalEnv expr  -- evaluate the righthand side
      modify $ first $ \oldBinds -> tryMatchLazy oldBinds pat rhs
      digest <- asks fst
      forM_ (boundNames pat) $ \i -> modify $ second
        $ IntMap.insert (identId i) (HashClass.mixInt digest $ identId i)
    FunBind i cases -> do
        finalEnv <- asks snd
        digest <- asks fst
        bindIdentM i $ VFun $ FunClosure {
          getFunCases = cases 
         ,getFunEnv = finalEnv
         ,getFunArgNum = length $ casePattern $ head cases
         ,getFunId  = mixInt digest $ AST.unNodeId $ AST.nodeId decl
         }
        where
          casePattern (FunCaseI pl _ ) = pl
          casePattern _ = throwInternalError "unexpected FunCase in AST" 
                             (Just $ srcLoc i) Nothing
    Assert {} -> return ()
    Transparent names ->  forM_ names $ \n -> bindIdentM n cspIdentityFunction
    SubType tname constrList -> do
{-
       subtypes are like data types except that we do not bind the constructs
       todo : check subtype declaration is correct, i.e. it really declares subtype
-}
       constrs <- mapM (constrDecl False) constrList
       bindIdentM tname (VDataType constrs )
    DataType tname constrList -> do
       constrs <- mapM (constrDecl True) constrList
       bindIdentM tname (VDataType constrs )
    NameType tname t -> do
      finalEnv <- asks snd
      bindIdentM tname (VNameType $ runEnv finalEnv $ evalTypeDef t)
    Print _expr -> return ()
    AST.Channel idList t -> do
      finalEnv <- asks snd
      forM_ idList $ \i -> bindIdentM i $ VChannel $ Types.Channel {
              chanId = AST.uniqueIdentId $ AST.unUIdent $ unLabel i
             ,chanName = AST.realName $ AST.unUIdent $ AST.unLabel i
             ,chanLen = case t of
                Nothing -> 1
                Just ty -> case unLabel ty of
                  TypeTuple _l -> 2
                  TypeDot l  -> length l+1
             ,chanFields = case t of
                Nothing -> []
                Just l -> runEnv finalEnv $ evalTypeDef l
             }

constrDecl :: Bool -> LConstructor -> DeclM Types.Constructor
constrDecl performBinding (unLabel -> AST.Constructor ident td) = do
  finalEnv <- asks snd
  let
    cl = case td of
      Nothing -> []
      Just l -> runEnv finalEnv $ evalTypeDef l

    constr = Types.Constructor
               (AST.uniqueIdentId $ AST.unUIdent $ unLabel ident)
               (AST.realName $ AST.unUIdent $ unLabel ident)
               cl 
  when performBinding $ bindIdentM ident $ VConstructor constr
  return constr

evalTypeDef :: LTypeDef -> EM [FieldSet] -- <- this is too restrictive ?
evalTypeDef t = case unLabel t of
  TypeDot l  -> mapM evalFieldSet l -- <- meight be a tuple of one
  TypeTuple l -> do
    el <- mapM evalFieldSet l
    -- cross-product
    return [SSet.fromList $ map VTuple $ sequence $ map SSet.toList el]

evalFieldSet :: LExp -> EM FieldSet
evalFieldSet expr = do
  v <- eval expr
  case v of
    VInt {} -> return $ SSet.singleton v
    VChannel {} -> return $ SSet.singleton v
    VSet s -> return $ SSet.Proper s
-- todo: Fix this when we have ClosureExpressions.
-- todo: This does not work for constructors that have fields.
    VDataType constrList -> return $ SSet.fromList $ map VConstructor constrList
    VNameType _ -> throwInternalError "nametype not implemented" (Just $ srcLoc expr) $ Just v
    VAllInts -> return $ SSet.fromList $ map VInt [0..10] --todo
    _ -> throwTypingError "evalFieldSet" (Just $ srcLoc expr) $ Just v

switchedOffProc :: LExp -> EM Process
switchedOffProc (unLabel -> ExprWithFreeNames free expr) = do
  env <- getEnv
  return $ Core.switchedOff $ SwitchedOffProc {
    switchedOffDigest = (closureDigest expr env free)
   ,switchedOffExpr = expr
   ,switchedOffProcess = runEM (evalProcess expr) env
   }
switchedOffProc expr
 = throwInternalError "cannot determine free variables" (Just $ srcLoc expr) Nothing

evalOutField :: LExp -> EM Field
evalOutField expr = do
  v <- eval expr
  case v of
    VInt {} -> return v
    VChannel {} -> return v
    VConstructor {} -> return v
    VTuple {} -> return v 
    VDotTuple {} -> return v -- todo : Fix for genric buffers
    VBool {} -> return v
{-
todo: Dupport lists and sets as channel fields.
Write test for VSet and VList.
-}
    VSet {} -> return v
    VList {} -> return v        

    _ -> throwTypingError "Eval.hs : evalOutField" (Just $ srcLoc expr) $ Just v


{- redo this: Most procComprehensions work on sets ! -}
evalProcCompL :: LExp -> LCompGenList -> EM [Process]
evalProcCompL p comp = evalListComp ret $ unLabel comp
  where
    ret = do
      r <- switchedOffProc p
      return [r]

{-
fdr does not remove duplicates from replicatesProc compostions,
see examples/CSP/FDRFeatureTests/ReplicatedInterleaveSetDef.csp
-}
evalProcCompS :: LExp -> LCompGenList -> EM [Process]
evalProcCompS = evalProcCompL
{-
evalProcCompS p comp
  =     (evalSetComp ret $ unLabel comp)
    >>= (mapM processFromValue) . Set.toList
  where
{-
We intermediateley wrap processes with VProcess.
If we make evalSetComp polymorphic we get the following error
src/Language/CSPM/Interpreter/Eval.hs:536:0:
    Contexts differ in length
      (Use -XRelaxedPolyRec to allow this)
-}
    ret = switchedOffProc p >>= return . Set.singleton . VProcess 
-}

evalListComp :: EM [x] -> [LCompGen] -> EM [x]
evalListComp ret [] = ret
evalListComp ret (h:t) = case unLabel h of
  Guard g -> do
    b <- evalBool g
    if b then evalListComp ret t
         else return []
  Generator pat gen -> do
    list <- evalList gen
    rets <- mapM (evalCompPat pat) list
    return $ concat rets
  where
    evalCompPat pat val = do
      e <- getEnv
      case tryMatchStrict (getArgBindings e) pat val of
        Nothing -> return []
        Just newBinds 
          -> return $ runEM
               (evalListComp ret t) 
               (setArgBindings e newBinds)

evalSetComp :: EM (Set Value) -> [LCompGen] -> EM (Set Value)
evalSetComp ret [] = ret
evalSetComp ret (h:t) = case unLabel h of
    Guard g -> do
      b <- evalBool g
      if b then evalSetComp ret t
           else return Set.empty
    Generator pat gen -> do
      set <- evalSet gen
      rets <- mapM (evalCompPat pat) $ Set.elems set
      return $ Set.unions rets
    where
      evalCompPat pat val = do
        e <- getEnv
        case tryMatchStrict (getArgBindings e) pat val of
          Nothing -> return Set.empty
          Just newBinds 
            -> return $ runEM
                 (evalSetComp ret t) 
                 (setArgBindings e newBinds)

evalAllEvents :: EM ClosureSet
evalAllEvents = do
  channels <- lookupAllChannels
  ClosureSet.mkEventClosure $ map VChannel channels

getSigma :: Env -> Sigma
getSigma = runEM evalAllEvents

cspIdentityFunction :: Value
cspIdentityFunction = VFun $ FunClosure {
   getFunCases = [funCase]
  ,getFunEnv = emptyEnvirionment
  ,getFunArgNum = 1
  ,getFunId = Hash.hash "cspIdentityFunction"
  }
  where
    funCase = FunCaseI [ labeled $ VarPat someId] (labeled $ Var someId)
    someId = labeled $ UIdent $ UniqueIdent {
      uniqueIdentId = -1
     ,bindingSide = e
     ,bindingLoc = e
     ,idType = e
     ,realName = e
     ,newName = e
     ,prologMode = e
     ,bindType = NotLetBound }
    e = throwInternalError "use identityFunction magic constants" Nothing Nothing

evalLinkList :: LLinkList -> EM RenamingRelation
evalLinkList l = case unLabel l of
  LinkList x -> liftM toRenaming $ mapM evalLink x
  LinkListComprehension gen links
    -> liftM toRenaming $ evalListComp (mapM evalLink links ) gen
  where
    evalLink :: LLink -> EM (Value,Value)
    evalLink (unLabel -> Link a b) = liftM2 (,) (eval a) (eval b)

functionCall :: Value -> [Value] -> EM (Value)
functionCall v arguments = case v of
  VFun fkt -> callFkt fkt arguments
  VPartialApplied fkt oldArgs -> callFkt fkt (oldArgs ++ arguments)
  f -> throwTypingError "calling non-function" Nothing $ Just f
  where
    tryFunCases :: [FunCase] -> [Value] -> Env -> Value
    tryFunCases [] _ _ = throwPatternMatchError "no matching function case" Nothing
    tryFunCases ((FunCaseI parameter fktBody) : moreCases) args env =
      case matchList parameter args (getArgBindings env) of
        Just newBinds -> runEval (setArgBindings env newBinds) fktBody
        Nothing -> tryFunCases moreCases args env
    tryFunCases (FunCase {} : _) _ _
      = throwInternalError "not expecting FunCase-Constructor" Nothing Nothing

    matchList :: [LPattern] -> [Value] -> Bindings -> Maybe Bindings
    matchList patList valList env
      = foldM (\e (pat,val) -> tryMatchStrict e pat val) 
         env (zip patList valList)

{-
  Going from
  callFkt fkt args = return $ tryFunCases (getFunCases fkt) args (getFunEnv fkt)
  to the version which supports partial application
  costs approx. 17 % in the fibonacci -example.
-}
    callFkt :: FunClosure -> [Value] -> EM Value
    callFkt fkt args
       = case compare haveArgs needArgs of
           EQ -> return $ tryFunCases (getFunCases fkt) args (getFunEnv fkt)
           GT -> do
             f2 <- callFkt fkt $ take needArgs args
             functionCall f2 $ drop needArgs args
           LT -> return $ VPartialApplied fkt args
       where
         haveArgs = length args
         needArgs = getFunArgNum fkt
