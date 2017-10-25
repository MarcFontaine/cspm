----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Fontaine 2009 - 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
--
-- Module      :  Scripting.LuaSrc
--
-- Concrete Lua source code that is included in the binary
----------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes #-}

module Scripting.LuaSrc
where

import Data.String.Quote

bootstrap :: String
bootstrap = [s|

|]

cspmPrelude :: String
cspmPrelude = [s|
local CSPM={}
local eval,transitions,viewProofTree,newTransistionList
local newProofTree,newValue,newProcess

function toString (o) return _cspm_toString(o) end
function valueToProcess(o) return _cspm_valueToProcess(o) end

function eval (expression, filename, verbose)
   local verb = verbose or false
   return newValue(_cspm_eval(verb, filename, expression))
end

function transitions (sigma, process)
   return newTransitionList(sigma, _cspm_transitions(sigma, process))
end

function viewProofTree (sigma, proofTree)
   local trans = _cspm_viewProofTree (proofTree)
   local res = {}
   res.event = toString (trans.event)
   res.nextState = newProcess (trans.succState, sigma)
   return res
end

function newTransitionList (sigma, rules)
   local trans = {}
   for key, rule in ipairs(rules) do
      trans[key] = newProofTree(sigma, rule)
   end
   return trans
end

function newProofTree (hsSigma, hsProof)
   local obj = viewProofTree (hsSigma, hsProof)
   obj.type = 'ProofTree'
   obj.toString = function () return toString(hsProof) end
   return obj
end

function newValue (obj)
   obj.type = 'Value'
   obj.toString = function () return toString(obj.value) end
   function obj.transitions ()
      return transitions(obj.sigma, valueToProcess(obj.value))
   end
   return obj
end

function newProcess (hsProc, hsSigma)
   local obj = {}
   obj.type  = 'Process'
   obj.toString = function () return toString(hsProc) end
   function obj.transitions ()
      return transitions(hsSigma, hsProc)
   end
   return obj
end


CSPM.Haskell_exports_info = _cspm_exportInfo(); _cspm_exportInfo = nil
CSPM.Haskell_exports = _cspm_hsExports; _cspm_hsExports = nil
CSPM.help = "cspmPrelude for the Lua interface of CSPM"
CSPM.eval = eval

return CSPM

|]

ilua :: String
ilua = [s|

|]

demo :: String
demo = [s|

|]
