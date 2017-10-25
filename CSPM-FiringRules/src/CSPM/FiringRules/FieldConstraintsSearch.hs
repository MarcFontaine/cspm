-----------------------------------------------------------------------------
-- |
-- Module      :  CSPM.FiringRules.FieldConstraintsSearch
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Field-wise generation of transitions.
-- Uses some kind of abstract interpretation/constraint propagation to avoid
-- enumeration of 'Sigma' in some cases.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module CSPM.FiringRules.FieldConstraintsSearch
(
  computeTransitions
 ,eventTransitions
 ,tauTransitions
 ,tickTransitions
)
where

import CSPM.CoreLanguage.Process
import qualified CSPM.CoreLanguage.Event as Event
import CSPM.CoreLanguage.Field as Field
import CSPM.FiringRules.Rules as Rules
import CSPM.FiringRules.Search

import Control.Arrow
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import qualified Data.List as List


computeTransitions ::  forall i. BF i 
  => Event.Sigma i -> Process i -> Search (Rule i)
computeTransitions events p
  =  (liftM EventRule $ eventTransitions events p)
         `mplus` (liftM TickRule $ tickTransitions p)
         `mplus` (liftM TauRule $ tauTransitions p)

data RuleField i
 = FPrefix (PrefixState i)
 | FExtChoiceL (RuleField i) (Process i)
 | FExtChoiceR (Process i) (RuleField i)
 | FExtChoice (RepExtChoicePart i) (RepExtChoicePart i)
 | FInterleaveL (RuleField i) (Process i)
 | FInterleaveR (Process i) (RuleField i)
 | FSeqNormal (RuleField i) (Process i)
 | FNotHidden (ClosureState i) (RuleField i)
 | FNotShareL (ClosureState i) (RuleField i) (Process i)
 | FNotShareR (ClosureState i) (Process i) (RuleField i)
 | FShared (ClosureState i) (RuleField i) (RuleField i)
 | FAParallelL (ClosureState i) (ClosureState i) (RuleField i) (Process i)
 | FAParallelR (ClosureState i) (ClosureState i) (Process i) (RuleField i)
 | FAParallelBoth (ClosureState i) (ClosureState i) (RuleField i) (RuleField i)
 | FNoInterrupt (RuleField i) (Process i)
 | FInterrupt (Process i) (RuleField i)
 | FTimeout (RuleField i) (Process i)
 | FRepAParallel (RepAP i)
 | FRenaming (Event.RenamingRelation i) (Process i)
 | FChaos (ClosureState i)
 | FLinkEventL (Event.RenamingRelation i) (RuleField i) (Process i)
 | FLinkEventR (Event.RenamingRelation i) (Process i) (RuleField i)
 | FNoException (ClosureState i) (RuleField i) (Process i)
 | FExceptionOccurs (ClosureState i) (Process i) (RuleField i)

rulePattern :: forall i.
  BF i => Event.EventSet i -> Process i -> Search (RuleField i)
rulePattern events proc = case proc of
  SwitchedOff p -> rp $ switchOn p
  Prefix p -> return $ FPrefix $ prefixStateInit ty p
  ExternalChoice p q
    -> joinRepExtChoiceParts
         (initRepExtChoicePart events p)
         (initRepExtChoicePart events q)
  InternalChoice _p _q -> mzero
  Interleave p q
    ->       (FInterleaveL <$> rp p <*> pure q)
     `mplus` (FInterleaveR p <$> rp q)
  Interrupt p q -> (FNoInterrupt <$> rp p <*> pure q)
     `mplus` (FInterrupt p <$> rp q)
  Timeout p q -> FTimeout <$> rp p <*> pure q
  Sharing p c q
    ->       (FShared (initClosure c) <$> rp p <*> rp q)
     `mplus` (FNotShareL (initClosure c) <$> rp p <*> pure q)
     `mplus` (FNotShareR (initClosure c) p <$> rp q)
  AParallel pc qc p q
    ->       (FAParallelL (initClosure pc) (initClosure qc) <$> rp p <*> pure q)
     `mplus` (FAParallelR (initClosure pc) (initClosure qc) <$> pure p <*> rp q)
     `mplus` (FAParallelBoth (initClosure pc) (initClosure qc) <$> rp p <*> rp q)
  Seq p q -> FSeqNormal <$> rp p <*> pure q
  Hide c p -> FNotHidden (initClosure c) <$> rp p
  Stop -> mzero
  Skip -> mzero
  Omega -> mzero
  AProcess _n -> mzero
  RepAParallel l -> return $ FRepAParallel $ initRepAParallel l
  Renaming rel p -> return $ FRenaming rel p
  Chaos c -> return $ FChaos $ initClosure c
  LinkParallel rel p q
    ->         (FLinkEventL rel <$> rp p <*> pure q)
       `mplus` (FLinkEventR rel p <$> rp q)
  Exception c p q
    ->         (FNoException (initClosure c) <$> rp p <*> pure q)
       `mplus` (FExceptionOccurs (initClosure c) p <$> rp q)
  where
    ty = (undefined :: i)
    initClosure = closureStateInit ty
    rp = rulePattern events    

type PropM i a = StateT (FieldSet i) Maybe a

propField :: forall i. BF i => RuleField i -> PropM i ()
propField rule = case rule of
  FPrefix p -> case viewPrefixState ty p of
    FieldOut f -> fixField f
    FieldIn -> return ()
    FieldGuard g -> restrictField $ \e -> intersection ty e g
  FExtChoiceL r _ -> propField r
  FExtChoiceR _ r -> propField r
  FExtChoice _p _q -> return ()
  FInterleaveL r _ -> propField r
  FInterleaveR _ r -> propField r
  FSeqNormal r _ -> propField r
  FNotHidden hidden r -> if closureState hidden == InClosure
    then impossibleRule
    else propField r
  FNotShareL c r _ -> if closureState c == InClosure
    then impossibleRule
    else propField r
  FNotShareR c _ r -> if closureState c == InClosure
    then impossibleRule
    else propField r
  FShared c r1 r2 -> if closureState c == NotInClosure
    then impossibleRule
    else do
      restrictField $ \e -> intersection ty e (closureFields c)
      propField r1
      propField r2
  FAParallelL c1 c2 r _ -> case (closureState c1,closureState c2) of
    (NotInClosure,_) -> impossibleRule
    (_,InClosure) -> impossibleRule
    _ -> do
      restrictField $ \e -> intersection ty e (closureFields c1)
      propField r
  FAParallelR c1 c2 _ r -> case (closureState c1,closureState c2) of
    (_,NotInClosure) -> impossibleRule
    (InClosure,_) -> impossibleRule
    _ -> do
      restrictField $ \e -> intersection ty e (closureFields c2)
      propField r
  FAParallelBoth c1 c2 r1 r2 -> case (closureState c1,closureState c2) of
    (NotInClosure,_) -> impossibleRule
    (_,NotInClosure) -> impossibleRule
    _ -> do
      restrictField $ \e -> intersection ty e (closureFields c1)
      restrictField $ \e -> intersection ty e (closureFields c2)
      propField r1
      propField r2
  FNoInterrupt r _ -> propField r
  FInterrupt _ r -> propField r
  FTimeout r _ -> propField r
  FRepAParallel RepAPFailed -> impossibleRule
  FRepAParallel x -> restrictField $ \e -> intersection ty e (repInitials x)
  FRenaming _ _  -> return () -- todo: some properagtion for renaming 
  FChaos c -> restrictField $ \e -> intersection ty e (closureFields c)
  FLinkEventL _ r _ -> propField r
  FLinkEventR _ _ r -> propField r
  FNoException c r _ -> if closureState c == InClosure
    then impossibleRule
    else propField r
  FExceptionOccurs c _ r -> if closureState c == NotInClosure
    then impossibleRule
    else propField r
  where
    restrictField :: (FieldSet i -> FieldSet i) -> PropM i ()
    restrictField fkt = do
      possible <- get
      let restricted = fkt possible
      if Field.null ty restricted
        then impossibleRule
        else put restricted

    fixField :: Field i -> PropM i ()
    fixField e = do
      possible <- get
      if member ty e possible
        then put $ singleton ty e
        else impossibleRule

    impossibleRule :: PropM i ()
    impossibleRule = mzero
    closureState :: ClosureState i -> ClosureView
    closureState = viewClosureState ty
    closureFields :: ClosureState i -> FieldSet i
    closureFields = viewClosureFields ty
    ty = (undefined :: i)

{-
Fix one field in the event.
-}
nextField :: forall i. BF i => RuleField i -> Field i -> Search (RuleField i)
nextField rule field = case rule of
  FPrefix p -> case prefixStateNext ty p field of
    Just a -> return $ FPrefix a
    Nothing -> mzero
  FExtChoiceL r p -> FExtChoiceL <$> rec r <*> pure p
  FExtChoiceR p r -> FExtChoiceR p <$> rec r
  FExtChoice p q
    -> joinRepExtChoiceParts
         (nextRepExtChoicePart p field)
         (nextRepExtChoicePart q field)
  FInterleaveL r p -> FInterleaveL <$> rec r <*> pure p
  FInterleaveR p r -> FInterleaveR p <$> rec r
  FSeqNormal r p -> FSeqNormal <$> rec r <*> pure p
  FNotHidden c r -> FNotHidden (fc c) <$> rec r
  FNotShareL c r p -> FNotShareL (fc c) <$> rec r <*> pure p
  FNotShareR c p r -> FNotShareR (fc c) p <$> rec r
  FShared c r1 r2 -> FShared (fc c) <$> rec r1 <*> rec r2
  FAParallelL c1 c2 r q
    -> FAParallelL (fc c1) (fc c2) <$> rec r <*> pure q
  FAParallelR c1 c2 p r
    -> FAParallelR (fc c1) (fc c2) p <$> rec r
  FAParallelBoth c1 c2 r1 r2
    -> FAParallelBoth (fc c1) (fc c2) <$> rec r1 <*> rec r2
  FNoInterrupt r q -> FNoInterrupt <$> rec r <*> pure q
  FInterrupt p r -> FInterrupt p <$> rec r
  FTimeout r q -> FTimeout <$> rec r <*> pure q
  FRepAParallel x -> return $ FRepAParallel $ repNextField field x
  FRenaming rel p -> return $ FRenaming rel p
  FChaos c -> return $ FChaos (fc c)
  FLinkEventL rel r q -> FLinkEventL rel <$> rec r <*> pure q
  FLinkEventR rel p r -> FLinkEventR rel p <$> rec r
  FNoException c r q -> FNoException (fc c) <$> rec r <*> pure q
  FExceptionOccurs c p r -> FExceptionOccurs (fc c) <$> pure p <*> rec r
  where
    rec r = nextField r field
    ty = (undefined :: i)
    fc c = closureStateNext ty c field

{-
Check constraints after last field and
convert RuleField to RuleEvent.
We must check all constraints here!
-}
lastField :: forall i. BF i
  => RuleField i -> Event.Event i -> Search (RuleEvent i)
lastField rule event = case rule of
  FPrefix p -> case prefixStateFinalize ty p of
    Nothing -> mzero
    Just x -> return $ HPrefix event x
  FExtChoiceL r p -> ExtChoiceL <$> rec r <*> pure p
  FExtChoiceR p r -> ExtChoiceR p <$> rec r
  FExtChoice (Right (p,rp)) (Right (q,rq))
    ->       (ExtChoiceL <$> (anyOf rp >>= rec) <*> pure q)
    `mplus`  (ExtChoiceR p <$> (anyOf rq >>= rec) )
  FExtChoice _ _ -> error "unreachable: this case is handled by nextField"
  FInterleaveL r p -> InterleaveL <$> rec r <*> pure p
  FInterleaveR p r -> InterleaveR p <$> rec r
  FSeqNormal r p -> SeqNormal <$> rec r <*> pure p
  FNotHidden hidden r -> do
    guard_not_inClosure hidden
    NotHidden (restoreClosure hidden) <$> rec r
  FNotShareL c r p -> do
    guard_not_inClosure c
    NotShareL (restoreClosure c) <$> rec r <*> pure p
  FNotShareR c p r -> do
    guard_not_inClosure c
    NotShareR (restoreClosure c) p <$> rec r
  FShared c r1 r2 -> do
    guard_inClosure c
    Shared (restoreClosure c) <$> rec r1 <*> rec r2
  FAParallelL c1 c2 r q -> case (inClosure c1,inClosure c2) of
    (True,False) -> AParallelL (restoreClosure c1) (restoreClosure c2) <$> rec r <*> pure q
    _ -> mzero
  FAParallelR c1 c2 p r -> case (inClosure c1,inClosure c2) of
    (False,True) -> AParallelR (restoreClosure c1) (restoreClosure c2) <$> pure p <*> rec r
    _ -> mzero
  FAParallelBoth c1 c2 r1 r2 ->  case (inClosure c1,inClosure c2) of
    (True,True) -> AParallelBoth (restoreClosure c1) (restoreClosure c2) 
                    <$> rec r1 <*> rec r2
    _ -> mzero
  FNoInterrupt r q -> NoInterrupt <$> rec r <*> pure q
  FInterrupt p r -> InterruptOccurs p <$> rec r
  FTimeout r q -> TimeoutNo <$> rec r <*> pure q
  FRepAParallel RepAPFailed -> mzero
  FRepAParallel x -> repToRules event x
  FRenaming rel p -> renamingRules rel p event
  FChaos c -> if inClosure c
    then return $ ChaosEvent (restoreClosure c) event
    else mzero
  FLinkEventL rel r q -> do
    guard $ not $ Event.isInRenamingDomain ty event rel
    LinkEventL rel <$> rec r <*> pure q
  FLinkEventR rel p r -> do
    guard $ not $ Event.isInRenamingRange ty event rel
    LinkEventR rel p <$> rec r
  FNoException c r p -> do
    guard_not_inClosure c
    NoException (restoreClosure c) <$> rec r <*> pure p
  FExceptionOccurs c p r -> do
    guard_inClosure c
    ExceptionOccurs (restoreClosure c) p <$> rec r
  where
    rec r = lastField r event
    ty = (undefined :: i)
    restoreClosure = closureRestore ty
    inClosure = seenPrefixInClosure ty
    guard_inClosure = guard . seenPrefixInClosure ty
    guard_not_inClosure = guard . not . seenPrefixInClosure ty

eventTransitions :: BF i => Event.EventSet i -> Process i -> Search (RuleEvent i)
eventTransitions events proc = liftM snd $ computeNextE events proc

computeNextE :: BF i 
  => Event.EventSet i -> Process i -> Search (Event.Event i, RuleEvent i)
computeNextE events proc = rulePattern events proc >>= runFields events

runFields :: forall i. BF i
  => Event.EventSet i -> RuleField i -> Search (Event.Event i, RuleEvent i)
runFields events r = do
      let baseEvents = closureStateInit ty events
      (chan,next) <- enumField (viewClosureFields ty baseEvents ) r
      (e,final) <- loopFields
         (closureStateNext ty baseEvents chan)
         [chan] -- the accumulator for fields
         next
         (channelLen ty chan -1)
      let event = joinFields ty $ reverse e
      rule <- lastField final event
      return (event,rule)
   where ty = (undefined :: i)

loopFields :: forall i.
  BF i =>
     ClosureState i   -- the universe for events
  -> [Field i]        -- accumulator for fields
  -> RuleField i      -- current rule
  -> Int              -- number fields left in prefix 
  -> Search ([Field i], RuleField i)
loopFields _ eventAcc rule 0 = return (eventAcc, rule)
loopFields closureState eventAcc rule n = do
      (f,next) <- enumField (viewClosureFields ty closureState) rule
      loopFields 
        (closureStateNext ty closureState f)
        (f:eventAcc)
        next
        (n-1)
   where ty = (undefined :: i)

enumField :: forall i. BF i => FieldSet i -> RuleField i -> Search (Field i, RuleField i)
enumField top r = case execStateT (propField r) top of
      Just s -> do
        f <- anyOf $ fieldSetToList ty s
        nr <- nextField r f
        return (f ,nr )
      Nothing -> mzero
  where ty = (undefined :: i)

tauTransitions :: forall i. BF i => Process i -> Search (RuleTau i)
tauTransitions proc = case proc of
  SwitchedOff p -> tauTransitions $ switchOn p
--  SwitchedOff p -> mzero
--  SwitchedOff p -> return $ TraceSwitchOn $ switchOn p
  Prefix {} -> mzero
  ExternalChoice p q
    ->       (ExtChoiceTauL <$> tauTransitions p <*> pure q)
     `mplus` (ExtChoiceTauR p <$> tauTransitions q)
  InternalChoice p q
    ->       (return $ InternalChoiceL p q)
     `mplus` (return $ InternalChoiceR p q)
  Interleave p q
    ->       (InterleaveTauL <$> tauTransitions p <*> pure q)
     `mplus` (InterleaveTauR p <$> tauTransitions q)
     `mplus` (InterleaveTickL <$> tickTransitions p <*> pure q)
     `mplus` (InterleaveTickR p <$> tickTransitions q)
  Interrupt p q
    ->       (InterruptTauL <$> tauTransitions p <*> pure q)
     `mplus` (InterruptTauR p <$> tauTransitions q)
  Timeout p q
    ->       (TimeoutTauR <$> tauTransitions p <*> pure q)
     `mplus` (return $ TimeoutOccurs p q)
  Sharing p c q
    ->       (ShareTauL c <$> tauTransitions p <*> pure q)
     `mplus` (ShareTauR c p <$> tauTransitions q)
     `mplus` (ShareTickL c <$> tickTransitions p <*> pure q)
     `mplus` (ShareTickR c p <$> tickTransitions q)
  AParallel pc qc p q
    ->       (AParallelTauL pc qc <$> tauTransitions p <*> pure q)
     `mplus` (AParallelTauR pc qc p <$> tauTransitions q)
     `mplus` (AParallelTickL pc qc <$> tickTransitions p <*> pure q)
     `mplus` (AParallelTickR pc qc p <$> tickTransitions q)
  Seq p q
    ->        (SeqTau <$> tauTransitions p <*> pure q)
      `mplus` (SeqTick <$> tickTransitions p <*> pure q)
  Hide hidden p -> (do
    rule <- (eventTransitions hidden p)
    return $ Hidden hidden rule)
   `mplus` (HideTau hidden <$> tauTransitions p)
  Stop -> mzero
  Skip -> mzero
  Omega -> mzero
  AProcess _n -> mzero
  RepAParallel _ -> mzero -- TODO ! tau for replicated AParallel
  Renaming rel p -> RenamingTau rel <$> tauTransitions p
  Chaos c -> return $ ChaosStop c
  LinkParallel rel p q
    ->        (LinkTauL rel <$> tauTransitions p <*> pure q)
      `mplus` (LinkTauR rel p <$> tauTransitions q)
      `mplus` (LinkTickL rel <$> tickTransitions p <*> pure q)
      `mplus` (LinkTickR rel p <$> tickTransitions q)
      `mplus` mkLinkedRules rel p q
  Exception c p q -> mzero -- TODO

tickTransitions :: BL i => Process i -> Search (RuleTick i)
tickTransitions proc = case proc of
  SwitchedOff p -> tickTransitions $ switchOn p
  Prefix {} -> mzero
  ExternalChoice p q
    ->       (ExtChoiceTickL <$> tickTransitions p <*> pure q)
     `mplus` (ExtChoiceTickR p <$> tickTransitions q)
  InternalChoice _p _q -> mzero
  Interleave Omega Omega -> return $ InterleaveOmega
  Interleave _ _ -> mzero
  Interrupt p q -> InterruptTick <$> tickTransitions p <*> pure q
  Timeout p q -> TimeoutTick <$> tickTransitions p <*> pure q
  Sharing Omega c Omega -> return $ ShareOmega c
  Sharing _ _ _ -> mzero
  AParallel c1 c2 Omega Omega -> return $ AParallelOmega c1 c2
  AParallel _ _ _ _ -> mzero
  RepAParallel l -> if all (isOmega . snd) l
    then return $ RepAParallelOmega $ map fst l
    else mzero
  Seq _p _q -> mzero
  Hide c p -> HiddenTick c <$> tickTransitions p
  Stop -> mzero
  Skip -> return $ SkipTick
  Omega -> mzero
  AProcess _n -> mzero
  Renaming rel p -> RenamingTick rel <$> tickTransitions p
  Chaos _ -> mzero
  LinkParallel rel Omega Omega -> return $ LinkParallelTick rel
  LinkParallel _ _ _ -> mzero
  Exception c p q -> mzero -- TODO

type RepAPProc i = (ClosureState i, Process i, [([Field.Field i], RuleEvent i)])
                                     -- why not do this field wise ^
data RepAP i
  = RepAP {
      repInitials :: FieldSet i
     ,repProcs :: [RepAPProc i]
     }
  | RepAPFailed

instance Show (RepAP i) where show _ = "RepAP"

initRepAParallel :: forall i. BF i
  => [(Event.EventSet i, Process i)]
  -> RepAP i
initRepAParallel l = RepAP {
   repInitials = joinInitials ln
  ,repProcs = ln
  }
  where
    ty = (undefined :: i)
    ln = map mkLn l
    mkLn :: (Event.EventSet i, Process i) -> RepAPProc i
    mkLn (closure,p)
      = (closureStateInit ty closure
        ,p
        ,map (first (splitFields ty)) $ runSearch $ computeNextE closure p)    

joinInitials :: forall i. BF i
  => [RepAPProc i]
  -> FieldSet i
joinInitials l= fieldSetFromList ty $ concatMap jf l where
  jf (_,_,a) = mapMaybe il a
  il ([],_) = Nothing
  il (h:_,_) = Just h
  ty = (undefined :: i)

repNextField :: forall i. BF i
  => Field i -> RepAP i -> RepAP i
repNextField _ RepAPFailed = RepAPFailed
repNextField field x = RepAP {
   repInitials = joinInitials newProcs
  ,repProcs = newProcs
  }
  where
    ty = (undefined :: i)
    newProcs :: [RepAPProc i]
    newProcs = map filterRules $ repProcs x
    filterRules :: RepAPProc i -> RepAPProc i
    filterRules (closure, p, rules) 
      = (closureStateNext ty closure field, p, mapMaybe nextR rules )
    nextR ([], _r) = Nothing
    nextR (h:t, r) | fieldEq ty field h = Just (t,r)
    nextR _ = Nothing

repToRules :: forall i. BF i 
  => Event.Event i
  -> RepAP i 
  -> Search (RuleEvent i)
repToRules event ra = do
  parts <- mapM mkPart $ repProcs ra
  if all isLeft parts
    then mzero
    else return $ RepAParallelEvent parts
  where
    mkPart :: (ClosureState i, Process i, [([Field.Field i], RuleEvent i)])
      -> Search (EventRepAPart i)
    mkPart (closure, origProc, []) = do
      guard (not $ inClosure closure)
      return $ Left (restoreClosure closure, origProc)
    mkPart (closure, _origProc, (map snd -> rules)) = do
      r <- anyOf rules
      return $ Right (restoreClosure closure, r)
    restoreClosure = closureRestore ty
    inClosure = seenPrefixInClosure ty   
    ty = (undefined :: i)
    isLeft (Left _) = True
    isLeft _ = False

{-
  todo : special cases for injective and relational renamings
-}    
renamingRules :: forall i. BF i
  => Event.RenamingRelation i
  -> Process i
  -> Event.Event i
  -> Search (RuleEvent i)
renamingRules rel proc event = do
    fromEvent <- anyOf $ Event.preImageRenaming ty rel event
    rule <- eventTransitions (Event.singleEventToClosureSet ty fromEvent) proc
    return $ Rename rel event rule    
  `mplus` (do
    guard $ not $ Event.isInRenamingDomain ty event rel
    -- here we could callback on enumNext !
    rule <- eventTransitions (Event.singleEventToClosureSet ty event) proc
    return $ RenameNotInDomain rel rule)
  where
    ty = (undefined :: i)

{-
We just enumerate everything,
very inefficient!
-}
mkLinkedRules :: forall i. BF i
   => Event.RenamingRelation i
   -> Process i
   -> Process i
   -> Search (RuleTau i)
mkLinkedRules rel p q = do
  (e1, r1) <- rules1
  (e2, r2) <- rules2
  guard $ Event.isInRenaming ty rel e1 e2
  return $ LinkLinked rel r1 r2
  where
    rules1 :: Search (Event.Event i, RuleEvent i)
    rules1 = rules (Event.getRenamingDomain ty rel) p
    rules2 = rules (Event.getRenamingRange ty rel) q
    rules :: [Event.Event i] -> Process i -> Search (Event.Event i, RuleEvent i)
    rules s proc = do
      e <- anyOf s
      -- Use EnumNext instead!
      computeNextE (Event.singleEventToClosureSet ty e) proc
    ty = (undefined :: i)


type RepExtChoicePart i = Either (Process i) (Process i,[RuleField i])

initRepExtChoicePart :: forall i. BF i
  => Event.EventSet i -> Process i -> RepExtChoicePart i
initRepExtChoicePart events p
  = if List.null rules
    then Left p
    else Right (p,rules)
  where rules = runSearch $ rulePattern events p

{-
nextRepExtChoicePart may call nextField with invalid fields.
nextRepExtChoicePart is only an approximation, it might return invalid rules.
-}
nextRepExtChoicePart :: forall i. BF i
  => RepExtChoicePart i -> Field i -> RepExtChoicePart i
nextRepExtChoicePart (Left p) _ = (Left p)
nextRepExtChoicePart (Right (p,rules)) field
{-
This is an error, we cannot rely on nextField to check the constraints
nextField might return invalid rules
-}
  = if List.null newRules
    then Left p
    else Right (p,newRules)
  where newRules = runSearch $ msum $ map (flip nextField field) rules

joinRepExtChoiceParts :: forall i. BF i
  => RepExtChoicePart i -> RepExtChoicePart i -> Search (RuleField i)
joinRepExtChoiceParts l r = case (l,r) of
  (Left _,Left _) -> mzero
  (Right (_,rules), Left q) -> FExtChoiceL <$> anyOf rules <*> pure q
  (Left p, Right (_,rules)) -> FExtChoiceR p <$> anyOf rules
  (Right _,Right _) -> return $ FExtChoice l r