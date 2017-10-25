----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.Interpreter.PrepareAST
-- Copyright   :  (c) Fontaine 2008-2011
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- These are preprocessing steps which are specific for the interpreter.
-- Those steps of general use are in the CSPM-Frontend-package
--
----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
module CSPM.Interpreter.PrepareAST
(
  prepareAST
)
where

import Language.CSPM.AST as AST
import qualified Language.CSPM.Frontend as Frontend
import CSPM.Interpreter.Types (INT)
import CSPM.Interpreter.PatternCompiler (compilePattern)

import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.Generics.Basics (Data)

type IModule = Frontend.ModuleFromRenaming

prepareAST :: Frontend.ModuleFromRenaming -> Module INT
prepareAST = compilePattern . replaceFunCase . addFreeNames
{-
ReplaceFunCase with funCaseNew.
This is a quickfix
In CSPM Syntax we have tree cases: fun(x)(y) fun(x,y) and fun((x,y))
we want to map them to : fun x y and fun (x,y) in Haskell-Syntax.
-}
replaceFunCase :: IModule -> IModule
replaceFunCase ast = everywhere (mkT compFC) ast
  where
    compFC :: FunCase -> FunCase
    compFC (FunCase args expr)= FunCaseI (concat args) expr
    compFC (FunCaseI _ _) 
      = error "Internal Error : Did not expect FunCaseI in parse result"
{-
    flatArgs args = case args of
      [x] -> x
      _     -> map wrapTuple args

    wrapTuple [a] = a  -- one-element lists are not Tuples ?
    wrapTuple x   =(AST.labeled . TuplePat) x
-}


-- | Perform a freename analyzis for the body of prefixOperations
-- | and expressions that can become process-closures.
-- | This is ugly !!.
addFreeNames :: IModule -> IModule
addFreeNames ast = everywhere trans ast
  where
    trans :: Data a => a-> a
    trans = mkT mkExp -- . mkT mkFunBind

    fn :: LExp -> LExp
    fn expr = setNode expr
        $ ExprWithFreeNames (Frontend.computeFreeNames expr) expr

{-
    mkFunBind :: Decl -> Decl
    mkFunBind (FunBind i c) = FunBindI i (Frontend.computeFreeNames c) c
    mkFunBind o = o
-}

    mkExp :: Exp -> Exp
    mkExp expr = case expr of
      Let decls e
        -> LetI decls (Frontend.computeFreeNames (decls,expr)) e
      Lambda p e
        -> LambdaI (Frontend.computeFreeNames (p,e)) p  e
      PrefixExp c f p
        -> PrefixI (Frontend.computeFreeNames (c,f,p) ) c f p
      ProcSharing s a b
        -> ProcSharing s (fn a) (fn b)
      ProcAParallel l r a b
        -> ProcAParallel l r (fn a) (fn b)
      ProcLinkParallel l a b
        -> ProcLinkParallel l (fn a) (fn b)
      ProcRenaming r gen p
        -> ProcRenaming r gen $ fn p
      ProcRepSequence l p 
        -> ProcRepSequence l $ fn p
      ProcRepInternalChoice l p
        -> ProcRepInternalChoice l $ fn p
      ProcRepInterleave l p
        -> ProcRepInterleave l $ fn p
      ProcRepExternalChoice l p
        -> ProcRepExternalChoice l $ fn p
      ProcRepAParallel l a p
        -> ProcRepAParallel l a $ fn p
      ProcRepLinkParallel l e p
        -> ProcRepLinkParallel l e $ fn p
      ProcRepSharing l e p
        -> ProcRepSharing l e $ fn p
-- this is really ugly
      CallBuiltIn x@(unLabel -> BuiltIn bi) [[a,b]]
          -> let constr = CallBuiltIn x in case bi of
        F_Sequential -> constr [[fn a,fn b]]
        F_Interrupt -> constr [[fn a,fn b]]
        F_ExtChoice -> constr [[fn a,fn b]]
        F_Timeout -> constr [[fn a,fn b]]
        F_IntChoice -> constr [[fn a,fn b]]
        F_Interleave -> constr [[fn a,fn b]]
        F_Hiding -> constr [[fn a,b]]
        _ -> constr [[a,b]]
      Fun2  x@(unLabel -> BuiltIn bi) a b
          -> let constr = Fun2 x in case bi of
        F_Sequential -> constr (fn a) (fn b)
        F_Interrupt -> constr (fn a) (fn b)
        F_ExtChoice -> constr (fn a) (fn b)
        F_Timeout -> constr (fn a) (fn b)
        F_IntChoice -> constr (fn a) (fn b)
        F_Interleave -> constr (fn a) (fn b)
        F_Hiding -> constr (fn a) b
        F_Guard -> constr a (fn b)
        _ -> constr a b
      other -> other
