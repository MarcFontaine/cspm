-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.CoreLanguage.FiringRules.Gen
-- Copyright   :  (c) Fontaine 2010
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Generation of arbitrary processes and proof trees for QuickCheck-testing.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CSPM.FiringRules.Test.Gen where

import CSPM.CoreLanguage
import CSPM.CoreLanguage.Event
import CSPM.FiringRules.Rules

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import qualified Data.List as List

class (BL i) => Arb i where
  genPrefix :: i -> Event i -> Gen (Prefix i)
  arbitraryEvent :: i -> Gen (Event i)
  arbitraryEventSet :: i -> Gen (EventSet i)
  arbitraryRenaming :: i -> Gen (RenamingRelation i)
  arbitraryRenaming ty = liftM (renamingFromList ty) $ arbitraryRenamingDR ty e e
    where e = allEvents ty
  arbitraryRenamingDR :: i -> EventSet i -> EventSet i -> Gen [(Event i, Event i)]
  arbitraryRenamingDR ty domain range
    = listOf1 $ liftM2 (,)
        (elements $ eventSetToList ty domain)
        (elements $ eventSetToList ty range)

mkArb :: Arbitrary a => (a -> t) -> Gen t
mkArb f = fmap f arbitrary

mkArb2 :: (Arbitrary a,Arbitrary b) => (a -> b -> t) -> Gen t
mkArb2 f = liftM2 f arbitrary arbitrary

mkArb3 ::
  (Arbitrary a, Arbitrary b, Arbitrary c) => 
  (a -> b -> c-> t) -> Gen t
mkArb3 f = liftM3 f arbitrary arbitrary arbitrary

instance Arb i => Arbitrary (Process i) where arbitrary = sized genProcess

arbitraryPrefix :: forall i. Arb i => i -> Gen (Prefix i)
arbitraryPrefix i = do
  arbitraryEvent i >>= genPrefix i

genProcess :: forall i. Arb i => Int -> Gen (Process i)
genProcess 0 = elements $ [Stop,Skip,Omega] ++ atomicProcesses
  where atomicProcesses = map AProcess [1..5]
genProcess n = frequency [
   (10, liftM Prefix $ arbitraryPrefix ty)
  ,(10, binProc ExternalChoice)
  ,(10, binProc InternalChoice)
  ,(10, binProc Interleave)
  ,(10, binProc Interrupt)
  ,(10, liftM3 Sharing subProcess (arbitraryEventSet ty) subProcess)
  ,(10, binProc Seq)
  ,(10, liftM2 Hide (arbitraryEventSet ty) arbitrary)
  ,(30, genProcess 0)
  ,(10, liftM3 Exception (arbitraryEventSet ty) subProcess subProcess)
  ]
  where
    binProc c = liftM2 c subProcess subProcess
    subProcess = resize (n `div` 2) arbitrary
    ty = (undefined :: i)

instance Arb i => Arbitrary (RuleEvent i) where arbitrary = arbitraryRE

arbitraryRE :: forall i. Arb i => Gen (RuleEvent i)
arbitraryRE = sized $ \size -> do
    arbitraryEvent (undefined :: i) >>= genRule size

instance Arb i => Arbitrary (RuleTau i) where
  arbitrary = sized genRuleTau

genRuleTau :: forall i. Arb i => Int -> Gen (RuleTau i)
genRuleTau 0 = oneof [ mkArb2 InternalChoiceL, mkArb2 InternalChoiceR ]
genRuleTau n = frequency [
     f10 $ do
        h <- arbSet
        hiddenEvent <- elements $ (eventSetToList ty) (allEvents ty)
        r <- genRule n hiddenEvent
        return $ Hidden (insert ty hiddenEvent h) r
    ,f10 $ ExtChoiceTauL <$> subRule <*> p
    ,f10 $ ExtChoiceTauR <$> p <*> subRule
    ,f10 $ SeqTick <$> subRuleTick <*> p
    ,f10 $ SeqTau <$> subRule <*> p
    ,f10 $ ShareTauL <$> arbSet <*> subRule <*> p
    ,f10 $ ShareTauR <$> arbSet <*> p <*> subRule
    ,f10 $ ShareTickL <$> arbSet <*> subRuleTick <*> p
    ,f10 $ ShareTickR <$> arbSet <*> p <*> subRuleTick
    ,f10 $ AParallelTauL <$> arbSet <*> arbSet <*> subRule <*> p
    ,f10 $ AParallelTauR <$> arbSet <*> arbSet <*> p <*> subRule
    ,f10 $ AParallelTickL <$> arbSet <*> arbSet <*> subRuleTick <*> p
    ,f10 $ AParallelTickR <$> arbSet <*> arbSet <*> p <*> subRuleTick
    ,f10 $ InterleaveTauL <$> subRule <*> p
    ,f10 $ InterleaveTauR <$> p <*> subRule
    ,f10 $ InterleaveTickL <$> subRuleTick <*> p
    ,f10 $ InterleaveTickR <$> p <*> subRuleTick
    ,f10 $ InterruptTauL <$> subRule <*> p
    ,f10 $ InterruptTauR <$> p <*> subRule
    ,f10 $ TimeoutTauR <$> subRule <*> p
    ,f10 $ TimeoutOccurs <$> p <*> p
    ,f10 $ ChaosStop <$> arbSet
    ,f10 $ LinkTauL <$> arbitraryRenaming ty <*> subRule <*> p
    ,f10 $ LinkTauR <$> arbitraryRenaming ty <*> p <*> subRule
    ,f10 $ LinkTickL <$> arbitraryRenaming ty <*> subRuleTick <*> p
    ,f10 $ LinkTickR <$> arbitraryRenaming ty <*> p <*> subRuleTick
    ,f10 $ do
       rel <- arbitraryRenaming ty
       (e1,e2) <- elements (renamingToList ty rel)
       LinkLinked rel <$> subRuleEvent e1 <*> subRuleEvent e2
    ]
  where
    subRule :: Gen (RuleTau i)
    subRule = genRuleTau $ n `div` 2
    subRuleTick :: Gen (RuleTick i)
    subRuleTick = genRuleTick $ n `div` 2
    subRuleEvent :: Event i -> Gen (RuleEvent i)
    subRuleEvent e = genRule (n `div` 2) e
    ty = (undefined :: i)
    p :: Gen (Process i)
    p = arbitrary
    f10 x = (10,x)
    arbSet = arbitraryEventSet ty

instance Arb i => Arbitrary (RuleTick i) where
  arbitrary = sized genRuleTick

genRuleTick :: forall i. Arb i => Int -> Gen (RuleTick i)
genRuleTick 0 = oneof
  [ liftM ShareOmega $ arbitraryEventSet (undefined :: i)
  , return InterleaveOmega ]

genRuleTick n = frequency [
     f10 $ return SkipTick
    ,f10 $ HiddenTick <$> arbSet <*> r
    ,f10 $ ShareOmega <$> arbSet
    ,f10 $ return InterleaveOmega
    ,f10 $ ExtChoiceTickL <$> r <*> p
    ,f10 $ ExtChoiceTickR <$> p <*> r
    ,f10 $ InterruptTick <$> r <*> p
    ,f10 $ TimeoutTick <$> r <*> p
    ,f10 $ RenamingTick <$> arbitraryRenaming ty <*> r
    ,f10 $ LinkParallelTick <$> arbitraryRenaming ty
    ,f10 $ AParallelOmega <$> arbSet <*> arbSet
    ,f10 $ RepAParallelOmega <$> (resize 4 $ listOf1 arbSet)
    ]
  where
    r :: Gen (RuleTick i)
    r = genRuleTick $ n `div` 2
    p :: Gen (Process i)
    p = arbitrary

    f10 x = (10,x)
    arbSet = arbitraryEventSet ty
    ty = (undefined :: i)

genRule :: forall i. Arb i => Int -> Event i -> Gen (RuleEvent i)
genRule 0 event = HPrefix event <$> genPrefix (undefined :: i) event
genRule n event = frequency [
     f10 $ ExtChoiceL <$> r <*> p
    ,f10 $ ExtChoiceR <$> p <*> r
    ,f10 $ InterleaveL <$> r <*> p
    ,f10 $ InterleaveR <$> p <*> r
    ,f10 $ SeqNormal <$> r <*> p
    ,f10 $ NotHidden <$> setWithoutEvent <*> genRule n event
    ,f10 $ NotShareL <$> setWithoutEvent <*> r <*> p
    ,f10 $ NotShareR <$> setWithoutEvent <*> p <*> r
    ,f10 $ Shared <$> setWithEvent <*> r <*> r
    ,f10 $ ChaosEvent <$> setWithEvent <*> pure event
    ,f10 $ AParallelL <$> setWithEvent <*> setWithoutEvent <*> r <*> p
    ,f10 $ AParallelR <$> setWithoutEvent <*> setWithEvent <*> p <*> r
    ,f10 $ AParallelBoth <$> setWithEvent <*> setWithEvent <*> r <*> r
    ,f10 $ arbitraryRepAParallelEvent
    ,f10 $ NoInterrupt <$> r <*> p
    ,f10 $ InterruptOccurs <$> p <*> r
    ,f10 $ TimeoutNo <$> r <*> p
    ,f10 $ do
       t <- arbitraryRenamingDR ty (allEvents ty) (allEvents ty)
       newEvent <- arbitraryEvent ty
       let rel = renamingFromList ty ((newEvent, event) : t)
       Rename rel event <$> genRule (n `div` 2) newEvent
    ,f10 $ do
       rel <- arbitraryRenamingDR ty
         (delete ty event (allEvents ty))
         (allEvents ty)
       RenameNotInDomain (renamingFromList ty rel) <$> r
    ,(30, genRule 0 event)
    ,f10 $ do
       rel <- arbitraryRenamingDR ty
         (delete ty event (allEvents ty))
         (allEvents ty)
       LinkEventL (renamingFromList ty rel) <$> r <*> p
    ,f10 $ do
       rel <- arbitraryRenamingDR ty
         (allEvents ty)
         (delete ty event (allEvents ty))
       LinkEventR (renamingFromList ty rel) <$> p <*> r
    ,f10 $ NoException <$> setWithoutEvent <*> r <*> p
    ,f10 $ ExceptionOccurs <$> setWithEvent <*> p <*> r
    ]
  where
    p :: Gen (Process i)
    p = arbitrary
    r :: Gen (RuleEvent i)
    r = genRule (n `div` 2) event

    f10 x = (10,x)
    arbSet = arbitraryEventSet ty
    setWithEvent :: Gen (EventSet i)
    setWithEvent = do
      x <- arbSet
      return $ insert ty event x
    setWithoutEvent :: Gen (EventSet i)
    setWithoutEvent = do
      x <- arbSet
      return $ delete ty event x
    ty = (undefined :: i)

    arbitraryRepAParallelEvent :: Gen (RuleEvent i)
    arbitraryRepAParallelEvent = do
      flags <- (resize 4 $ listOf1 arbitrary) -- hardcoded max 4 processes
            `suchThat` List.or
      RepAParallelEvent <$> mapM repAPart flags
    repAPart :: Bool -> Gen (EventRepAPart i)
    repAPart True  = Right <$> liftM2 (,) setWithEvent r
    repAPart False = Left <$> liftM2 (,) setWithoutEvent p
