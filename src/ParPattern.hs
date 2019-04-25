{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
module ParPattern where

import           Control.Monad
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Tuple
import           Data.Kind
import           Data.Type.Natural
import           Data.Maybe
import           Type.Reflection

import           Language.Poly.Core
import           RtDef

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 >>>, <<<

data Pipe a (b  :: Type) =
  (Serialise a, Serialise b) => Pipe
    { start :: (Nat, TypeRep a)
    , cont  :: AProcRTFunc a
    , env   :: Map Nat AProcRT
    , end   :: (Nat, TypeRep b)
    }

type ArrowPipe a b = Nat -> Pipe a b

arrowId :: Serialise a => ArrowPipe a a
arrowId = arr Id

-- >>>
-- [r0 --- r1] >>> [r1 ---- ..]
(>>>) :: (ArrowPipe a b) -> (ArrowPipe b c) -> (ArrowPipe a c)
(>>>) leftArrow rightArrow sender = compose leftP rightP
 where
  leftP  = leftArrow sender
  -- continue with the last node in the lef branch
  rightP = rightArrow (getMaximum leftP)

(<<<) :: (ArrowPipe b c) -> (ArrowPipe a b) -> ArrowPipe a c
(<<<) = flip (>>>)

-- arr
-- introduce new role f is ad-hoc function
-- don't introduce new role for, inl, inr, fst, snd
arr :: (Serialise a, Serialise b) => (Core (a -> b)) -> ArrowPipe a b
arr (f :: Core (a -> b)) sender = case f of
  Fst -> withOutNewRole
  Snd -> withOutNewRole
  Inl -> withOutNewRole
  Inr -> withOutNewRole
  Id  -> withOutNewRole
  _   -> withNewRole
 where
  receiver    = sender + 1
  withNewRole = Pipe { start = (sender, typeRep)
                     , cont  = procSend
                     , env   = Map.singleton receiver procRecv
                     , end   = (receiver, typeRep)
                     }
  procRecv       = toAProc (recv' sender >>= return . (f :$))
  procSend       = toAProcRTFunc (\x -> send' receiver x)

  withOutNewRole = Pipe { start = (sender, typeRep :: TypeRep a)
                        , cont  = toAProcRTFunc (return . (f :$))
                        , env   = Map.empty
                        , end   = (sender, typeRep :: TypeRep b)
                        }

arrowFirst :: Serialise d => ArrowPipe b c -> ArrowPipe (b, d) (c, d)
arrowFirst = (*** arrowId)

arrowSecond :: Serialise d => ArrowPipe b c -> ArrowPipe (d, b) (d, c)
arrowSecond = (arrowId ***)

-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
-- start a new role in left branch, the right branch containue wth leftMax + 1
-- in the end send back to the rightMax
(***) :: (ArrowPipe b c) -> (ArrowPipe b' c') -> ArrowPipe (b, b') (c, c')
(***) leftArrow rightArrow sender = starHelper leftP rightP sender
 where
  leftP      = leftArrow (sender + 1)
  rightStart = (getMaximum leftP) + 1
  rightP     = rightArrow rightStart

-- (&&&) :: a b c -> a b c' -> a b (c, c')
(&&&) :: (ArrowPipe b c) -> (ArrowPipe b c') -> ArrowPipe b (c, c')
-- (&&&) leftArrow rightArrow sender receiver = undefined
(&&&) leftArrow rightArrow sender = andHelper leftP rightP sender
 where
  leftP      = leftArrow (sender + 1)
  rightStart = (getMaximum leftP) + 1
  rightP     = rightArrow rightStart

-- (+++)
-- notice the difference is we start with the right one
-- rightStart = sender  (not sneder + 1)
-- leftStart = rightMax + 1
(+++)
  :: (ArrowPipe a c) -> (ArrowPipe b d) -> Nat -> Pipe (Either a b) (Either c d)
(+++) leftArrow rightArrow sender = plusHelper leftP rightP sender
 where
  rightP = rightArrow (sender + 1)
  leftP  = leftArrow (1 + getMaximum rightP)

-- (|||)
(|||) :: (ArrowPipe a c) -> (ArrowPipe b c) -> ArrowPipe (Either a b) c
(|||) leftArrow rightArrow sender = barHelper leftP rightP sender
 where
  rightP = rightArrow (sender + 1)
  leftP  = leftArrow (1 + getMaximum rightP)

arrowLeft :: Serialise d => ArrowPipe b c -> ArrowPipe (Either b d) (Either c d)
arrowLeft = (+++ arrowId)

arrowRight
  :: Serialise d => ArrowPipe b c -> ArrowPipe (Either d b) (Either d c)
arrowRight = (arrowId +++)

-- execution
runPipe :: Nat -> Core a -> (ArrowPipe a b) -> [ProcessRT ()]
runPipe start x f = runPipe' (f start) x

-- helper function zone
compose :: Pipe a b -> Pipe b c -> Pipe a c
compose first@Pipe{} second@Pipe{}
  | sender == receiver = helper eitherProcOrFunc
  | otherwise          = error "compose"
 where
  sender           = endNat first
  receiver         = startNat second

  -- get the proc belongs to ther sender (the last role in the first) 
  sendProc         = getAProcRTofEndSafe first
  mergedEnv        = Map.unionWith bind (env first) (env second)

  -- deal with the 1. sender is not found in the first pipe 2. sender is found
  eitherProcOrFunc = case sendProc of
    Just proc ->
      Left
        $ let newProc = proc `bind4` (cont second)
          in  maybe newProc (newProc `bind`) $ Map.lookup sender $ env second
    Nothing -> if startNat first == endNat first
      then Right $ (cont first) `bind3` (cont second)
      else error "sender process in the first pipe not found"

  helper (Left proc) = Pipe { start = start first
                            , cont  = cont first
                            , env   = Map.insert sender proc mergedEnv
                            , end   = end second
                            }
  helper (Right procFunc) = Pipe { start = start first
                                 , cont  = procFunc
                                 , env   = mergedEnv
                                 , end   = end second
                                 }

-- only did optimization for one cases
arrowProd
  :: Serialise e
  => (Core e -> Core b)
  -> (Core e -> Core b')
  -> Pipe b c
  -> Pipe b' c'
  -> Nat
  -> Pipe e (c, c')
arrowProd (fl :: Core e -> Core b) fr (leftP@Pipe{} :: Pipe b c) (rightP@Pipe{} :: Pipe
    b'
    c') sender
  | receiver == rightEndSend && (sender /= receiver)
  = tmpPipe
  | otherwise
  = error "arrowProd"
 where
  receiver     = getMaximum rightP
  leftRecv     = startNat leftP
  rightRecv    = startNat rightP
  leftEndSend  = endNat leftP
  rightEndSend = endNat rightP

  procSend =
    toAProcRTFunc (\x -> send' leftRecv (fl x) >> send' rightRecv (fr x))

  -- before mergeing the recvProc command
  tmpPipe = Pipe
    { start = (sender, typeRep :: TypeRep e)
    , cont  = procSend
    , env   = Map.update (\x -> Just $ (x `bind4` recvProc)) receiver
                $ Map.unionWith bind leftEnv rightEnv
    , end   = (receiver, typeRep :: TypeRep (c, c'))
    }

  leftEnv =
    updateEnvWithSendProcSafe receiver leftEndSend
      $ updateEnvWithRecvProc sender leftP
  rightEnv = updateEnvWithRecvProc sender rightP

  recvProc = toAProcRTFunc $ \(y :: Core c') -> do
    x :: Core c <- recv' leftEndSend
    return (Pair x y)

arrowSum
  :: Serialise e
  => (Core c -> Core e)
  -> (Core d -> Core e)
  -> Pipe a c
  -> Pipe b d
  -> Nat
  -> Pipe (Either a b) e
arrowSum (fl :: Core c -> Core e) fr (leftP@Pipe{} :: Pipe a c) (rightP@Pipe{} :: Pipe
    b
    d) sender
  | receiver == leftEndSend && sender + 1 == rightRecv
  = Pipe { start = (sender, typeRep)
         , cont  = procSend
         , env   = newEnv
         , end   = (receiver, typeRep)
         }
  | otherwise
  = error "arrowSum"
 where
  leftRecv     = startNat leftP
  rightRecv    = startNat rightP
  leftEndSend  = endNat leftP
  rightEndSend = endNat rightP
  receiver     = getMaximum leftP

  allRole =
    Set.toList $ Set.union (getAllRoles leftP) (getAllRoles rightP)
  procSend = toAProcRTFunc
    (\x -> selectMulti' allRole
                        (x :: Core (Either a b))
                        (ignoreOutput . send' leftRecv)
                        (ignoreOutput . send' rightRecv)
    )
  applyFl = toAProcRTFunc $ (\x -> return $ fl x)
  -- since receiver is the same as the leftMax
  leftEnv =
    Map.update (\x -> Just $ x `bind4` applyFl) receiver
      $ updateEnvWithRecvProc sender leftP
  rightEnv =
    updateEnvWithSendProcWith fr receiver (endNat rightP)
      $ updateEnvWithRecvProc sender rightP

  -- procs that do both branches
  dupEnv = Map.intersectionWith (addBranch sender) leftEnv rightEnv
  -- procs that do action in the left branch, not in the right branch
  -- the receiver at left right branch is the recv from right end
  newLeftEnv =
    Map.mapWithKey
        (\key x -> if key /= receiver
          then addBranch sender x (toAProc $ return $ Lit ())
          else addBranch sender x (toAProc $ (recv' rightEndSend :: ProcRT e))
        )
      $ Map.difference leftEnv rightEnv
  -- procs that do action in the right branch, not in the left branch
  newRightEnv =
    Map.map (\x -> addBranch sender (toAProc $ return $ Lit ()) x)
      $ Map.difference rightEnv leftEnv

  newEnv = Map.union dupEnv $ Map.union newLeftEnv newRightEnv

starHelper :: Pipe b c -> Pipe b' c' -> ArrowPipe (b, b') (c, c')
starHelper leftP@Pipe{} rightP@Pipe{} =
  arrowProd (Fst :$) (Snd :$) leftP rightP

andHelper :: Pipe b c -> Pipe b c' -> ArrowPipe b (c, c')
andHelper leftP@Pipe{} rightP@Pipe{} = arrowProd id id leftP rightP

plusHelper :: Pipe a c -> Pipe b d -> ArrowPipe (Either a b) (Either c d)
plusHelper leftP@Pipe{} rightP@Pipe{} = arrowSum (Inl :$) (Inr :$) leftP rightP

barHelper :: Pipe a c -> Pipe b c -> ArrowPipe (Either a b) c
barHelper leftP@Pipe{} rightP@Pipe{} = arrowSum id id leftP rightP

runPipe' :: Pipe a b -> Core a -> [ProcessRT ()]
runPipe' pipe val = toProcessRT $ Map.insertWith bind key procVal (env pipe)
 where
  key      = fst $ start pipe
  procFunc = cont pipe
  procVal  = callAProcRTFunc procFunc val
  toProcessRT procMap =
    let f (AProcRT _ proc) = ignoreOutput proc
    in  (swap . fmap f) <$> Map.toList procMap

-- utility functions
getAllRoles :: Pipe a b -> Set Nat
getAllRoles p@Pipe {..} = Set.insert (startNat p) $ Map.keysSet env

getMaximum :: Pipe a b -> Nat
getMaximum p@Pipe {..} = case Map.lookupMax env of
  Just (k, _) -> max k $ startNat p
  Nothing     -> startNat p

callAProcRTFunc :: AProcRTFunc a -> Core a -> AProcRT
callAProcRTFunc (AProcRTFunc ty cont) val = AProcRT ty (cont val)

getAProcRTofEnd :: Pipe a b -> AProcRT
getAProcRTofEnd p@Pipe {..} =
  fromMaybe (error "no proc for end") $ getAProcRTofEndSafe p

getAProcRTofEndSafe :: Pipe a b -> Maybe AProcRT
getAProcRTofEndSafe p@Pipe {..} = getAProcRTSafe p (fst end)

getAProcRTSafe :: Pipe a b -> Nat -> Maybe AProcRT
getAProcRTSafe Pipe {..} role = Map.lookup role env

addRecv :: AProcRTFunc a -> Nat -> AProcRT
addRecv ((AProcRTFunc ty cont) :: AProcRTFunc a) sender = AProcRT ty wholeProc
 where
  recvProc  = recv' sender :: ProcRT a
  wholeProc = recvProc >>= cont

addSend :: AProcRT -> Nat -> AProcRT
addSend (AProcRT (ty :: TypeRep a) proc) receiver = AProcRT ty (proc >>= cont)
  where cont = send' receiver :: Core a -> ProcRT a

addSendFunc :: AProcRTFunc b -> Nat -> AProcRTFunc b
addSendFunc (AProcRTFunc (ty :: TypeRep a) func) receiver = AProcRTFunc
  ty
  (func >=> cont)
  where cont = send' receiver :: Core a -> ProcRT a

addSendWith
  :: (Serialise a, Serialise b)
  => (Core a -> Core b)
  -> AProcRT
  -> Nat
  -> AProcRT
addSendWith (f :: Core a -> Core b) (AProcRT (ty :: TypeRep c) proc) receiver =
  case ty `eqTypeRep` rep of
    Just HRefl -> AProcRT rep2 (proc >>= \x -> send' receiver (f x))
    Nothing    -> error "typed mismatched in addSend"
 where
  rep  = typeRep :: TypeRep a
  rep2 = typeRep :: TypeRep b

getRecvProc :: Nat -> Pipe a b -> AProcRT
getRecvProc sender p@Pipe {..} = procRecv
 where
  receiver         = startNat p
  procRecv         = addRecv combinedProcFunc sender
  combinedProcFunc = case Map.lookup receiver env of
    Just proc -> cont `bind2` proc
    Nothing   -> cont

updateEnvWithRecvProc :: Nat -> Pipe a b -> Map Nat AProcRT
updateEnvWithRecvProc sender pipe@Pipe {..} =
  Map.insert (startNat pipe) (getRecvProc sender pipe) env

getProcFromEnv :: Nat -> Map Nat AProcRT -> AProcRT
getProcFromEnv key env =
  fromMaybe (error "proc doesn't exist in env") (Map.lookup key env)

startNat :: Pipe a b -> Nat
startNat = fst . start

endNat :: Pipe a b -> Nat
endNat = fst . end

getSendProc :: Nat -> Nat -> Map Nat AProcRT -> AProcRT
getSendProc receiver sender env = addSend (getProcFromEnv sender env) receiver

getSendProcWith
  :: (Serialise a, Serialise b)
  => (Core a -> Core b)
  -> Nat
  -> Nat
  -> Map Nat AProcRT
  -> AProcRT
getSendProcWith (f :: Core a -> Core b) receiver sender env
  | receiver /= sender = addSendWith f (getProcFromEnv sender env) receiver
  | otherwise = case getProcFromEnv sender env of
    AProcRT ty procRt -> case ty `eqTypeRep` ty' of
      Just HRefl -> AProcRT rep (procRt >>= return . f)
      Nothing    -> error "f cannot apply to return value of proc"
 where
  rep = typeRep :: TypeRep b
  ty' = typeRep :: TypeRep a

-- if sender doesn't present in the env, then return the unmodified env
updateEnvWithSendProcSafe :: Nat -> Nat -> Map Nat AProcRT -> Map Nat AProcRT
updateEnvWithSendProcSafe receiver sender env = case Map.lookup sender env of
  Just _  -> Map.insert sender (getSendProc receiver sender env) env
  Nothing -> env

updateEnvWithSendProcWith
  :: (Serialise a, Serialise b)
  => (Core a -> Core b)
  -> Nat
  -> Nat
  -> Map Nat AProcRT
  -> Map Nat AProcRT
updateEnvWithSendProcWith f receiver sender env =
  Map.insert sender (getSendProcWith f receiver sender env) env

addBranch :: Nat -> AProcRT -> AProcRT -> AProcRT
addBranch sender (AProcRT (tyl :: TypeRep a) procl) (AProcRT (tyr :: TypeRep b) procr)
  = case tyl `eqTypeRep` tyr of
    Just HRefl -> toAProc $ branchCont' sender procl procr
    Nothing ->
      toAProc $ branch' sender (ignoreOutput procl) (ignoreOutput procr)

bind :: AProcRT -> AProcRT -> AProcRT
bind (AProcRT _ proc1) (AProcRT ty2 proc2) = AProcRT ty2 (proc1 >> proc2)

bind2 :: AProcRTFunc a -> AProcRT -> AProcRTFunc a
bind2 (AProcRTFunc _ func) (AProcRT ty2 proc) =
  AProcRTFunc ty2 ((>> proc) . func)

bind3 :: AProcRTFunc a -> AProcRTFunc b -> AProcRTFunc a
bind3 (AProcRTFunc ty func) (AProcRTFunc ty2 (func2 :: Core c -> ProcRT b)) =
  case ty `eqTypeRep` rep of
    Just HRefl -> AProcRTFunc ty2 (func >=> func2)
    Nothing    -> error "Two AProcRTFunc are not compatible"
  where rep = typeRep :: TypeRep c

bind4 :: AProcRT -> AProcRTFunc a -> AProcRT
bind4 (AProcRT ty proc) (AProcRTFunc ty2 (func :: Core c -> ProcRT b)) =
  case ty `eqTypeRep` rep of
    Just HRefl -> AProcRT ty2 (proc >>= func)
    Nothing    -> error "AProcRT and AProcRTFunc are not compatible"
  where rep = typeRep :: TypeRep c

toAProc :: Serialise a => ProcRT a -> AProcRT
toAProc (val :: ProcRT a) = AProcRT (typeRep :: TypeRep a) val

toAProcRTFunc
  :: (Serialise a, Serialise b) => (Core a -> ProcRT b) -> AProcRTFunc a
toAProcRTFunc (f :: Core a -> ProcRT b) = AProcRTFunc (typeRep :: TypeRep b) f

getRecvProcOrFunc :: Pipe a b -> Nat -> Either AProcRT (AProcRTFunc a)
getRecvProcOrFunc pipe@Pipe {..} receiver =
  case getAProcRTSafe pipe receiver of
    Just proc -> Left proc
    Nothing   -> if startNat pipe == endNat pipe
      then Right cont
      else error "mistake can't find"
