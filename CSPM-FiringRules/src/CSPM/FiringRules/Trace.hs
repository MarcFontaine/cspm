-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.Trace
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- A very rudimentary process tracer for debugging and testing.
-- Prints the current process and the possible transitions to stdout
-- and lets the user select a transition by typing to stdin.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module CSPM.FiringRules.Trace
where

import CSPM.CoreLanguage

import CSPM.FiringRules.Rules
import CSPM.FiringRules.Verifier  (viewProcAfter,viewEvent)
import CSPM.FiringRules.FieldConstraints (computeTransitions)
import CSPM.FiringRules.HelperClasses

-- | A simplistic interactive CSP-M tracer.
-- It prints all events offered by a Proccess to stdout and
-- promts the user for the event to perform.
trace :: forall i e. (FShow i, ShowTTE e , CSP2 i, e ~ TTE i)
  => Sigma i -> Process i -> IO ()
trace events process = do
--  putStrLn "Process :"
--  putStr $ show $ hash process
  let rules = computeTransitions events process
  if null rules
    then putStrLn "deadlock state"
    else do
      sequence_ $ zipWith printTrans [0..] rules
      putStrLn "Select a Transition "
      i <- readLn
      trace events (viewProcAfter (rules !! i))
  where
    printTrans :: Int -> Rule i -> IO ()
    printTrans nr r = do
      putStr (show nr ++ " : ")
      putStrLn ""
--      putStrLn $ show r  
      putStr $ showTTE $ viewEvent r
      putStrLn ""