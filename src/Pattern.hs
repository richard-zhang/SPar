{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
module Pattern where

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

-- recv from the 1, split (usually) and send left part to 2, right part to thrid to 3
type Divider = Nat -> Nat -> Nat -> ProcRT ()

-- recv left from 2, right from 3, combine and send to 1
type Combiner = Nat -> Nat -> Nat -> ProcRT ()

-- recv from 1, send to 2
type Basic = Nat -> Nat -> ProcRT ()

data AProcRT where
  AProcRT
    :: forall a. Serialise a
    => TypeRep a
    -> ProcRT a
    -> AProcRT

data AProcRTFunc a where
  AProcRTFunc
    :: forall a c. (Serialise a, Serialise c)
    => TypeRep c
    -> (Core a -> ProcRT c)
    -> AProcRTFunc a

data Pipe a (b  :: Type) =
  (Serialise a, Serialise b) => Pipe
    { start :: (Nat, TypeRep a)
    , cont  :: AProcRTFunc a
    , env   :: Map Nat AProcRT
    , end   :: (Nat, TypeRep b)
    }

--  id
arrowId :: Serialise a => Nat -> Nat -> Pipe a a
arrowId = arr Id

-- >>>
(>>>)
  :: (Nat -> Nat -> Pipe a b)
  -> (Nat -> Nat -> Pipe b c)
  -> (Nat -> Nat -> Pipe a c)
(>>>) leftArrow rightArrow sender receiver = compose' leftP rightP
 where
  leftP  = leftArrow sender sender
  rightP = rightArrow receiver receiver

(<<<)
  :: (Nat -> Nat -> Pipe b c)
  -> (Nat -> Nat -> Pipe a b)
  -> Nat
  -> Nat
  -> Pipe a c
(<<<) = flip (>>>)

-- arr
arr :: (Serialise a, Serialise b) => (Core (a -> b)) -> Nat -> Nat -> Pipe a b
arr f sender receiver
  | sender /= receiver = Pipe { start = (sender, typeRep)
                              , cont  = procSend
                              , env   = Map.singleton receiver procRecv
                              , end   = (receiver, typeRep)
                              }
  | otherwise = Pipe { start = (sender, typeRep)
                     , cont  = toAProcRTFunc (return . (f :$))
                     , env   = Map.empty
                     , end   = (receiver, typeRep)
                     }
 where
  procRecv = toAProc (recv' sender >>= return . (f :$))
  procSend = toAProcRTFunc (\x -> send' receiver x)

-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
(***)
  :: (Nat -> Nat -> Pipe b c)
  -> (Nat -> Nat -> Pipe b' c')
  -> Nat
  -> Nat
  -> Pipe (b, b') (c, c')
(***) leftArrow rightArrow sender receiver = generalMerger' leftP
                                                            rightP
                                                            sender
                                                            receiver
 where
  leftP  = leftArrow (S sender) (S sender)
  rightP = rightArrow (S $ getMaximum leftP) (S $ getMaximum leftP)

-- (&&&) :: a b c -> a b c' -> a b (c, c')
(&&&)
  :: (Nat -> Nat -> Pipe b c)
  -> (Nat -> Nat -> Pipe b c')
  -> Nat
  -> Nat
  -> Pipe b (c, c')
-- (&&&) leftArrow rightArrow sender receiver = undefined
(&&&) leftArrow rightArrow sender receiver = mymerger' leftP
                                                       rightP
                                                       sender
                                                       receiver
 where
  leftP  = leftArrow (S sender) (S sender)
  rightP = rightArrow (S $ getMaximum leftP) (S $ getMaximum leftP)

-- (+++)
(+++)
  :: (Nat -> Nat -> Pipe a c)
  -> (Nat -> Nat -> Pipe b d)
  -> Nat
  -> Nat
  -> Pipe (Either a b) (Either c d)
(+++) leftArrow rightArrow sender receiver = generalSelector' leftP
                                                              rightP
                                                              sender
                                                              receiver
 where
  leftP  = leftArrow (S sender) receiver
  rightP = rightArrow (S sender) receiver

-- (|||)
(|||)
  :: (Nat -> Nat -> Pipe a c)
  -> (Nat -> Nat -> Pipe b c)
  -> Nat
  -> Nat
  -> Pipe (Either a b) c
(|||) leftArrow rightArrow sender receiver = selector' leftP
                                                       rightP
                                                       sender
                                                       receiver
 where
  leftP  = leftArrow (S sender) (S sender)
  rightP = rightArrow (S $ getMaximum leftP) (S $ getMaximum leftP)

-- execution
runPipe :: (Nat -> Nat -> Pipe a b) -> Nat -> Nat -> Core a -> [ProcessRT ()]
runPipe f start finish = runPipe' $ f start finish

-- helper function zone
compose' :: Pipe a b -> Pipe b c -> Pipe a c
compose' first@Pipe{} second@Pipe{}
  | sender /= receiver && not isSenderOnlyInProcFunc = Pipe
    { start = start first
    , cont  = cont first
    , env   = newEnv
    , end   = end second
    }
  | sender /= receiver && isSenderOnlyInProcFunc = Pipe
    { start = start first
    , cont  = addSendFunc (cont first) receiver
    , env   = newEnv
    , end   = end second
    }
 where
  -- assume sender receiver are different for now
  isSenderOnlyInProcFunc =
    (isNothing $ Map.lookup sender (env first)) && (sender == startNat first)

  sender    = endNat first
  receiver  = startNat second

  -- union algorithm when duplicate is to add proc in the right at the end of proc in the left
  firstEnv  = updateEnvWithSendProcSafe receiver sender (env first)
  secondEnv = updateEnvWithRecvProc sender second
  newEnv    = Map.unionWith bind firstEnv secondEnv
compose' first@Pipe{} second@Pipe{} | otherwise = helper eitherProcOrFunc
 where
  sender           = endNat first
  sendProc         = getAProcRTofEndSafe first
  mergedEnv        = Map.unionWith bind (env first) (env second)

  -- deal with the case 1. sender is not found in the first pipe 2. sender is found
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
mergeWith
  :: Serialise e
  => (Core e -> Core b)
  -> (Core e -> Core b')
  -> Pipe b c
  -> Pipe b' c'
  -> Nat
  -> Nat
  -> Pipe e (c, c')
mergeWith (fl :: Core e -> Core b) fr (leftP@Pipe{} :: Pipe b c) (rightP@Pipe{} :: Pipe
    b'
    c') sender receiver
  | isOptimized
  = undefined
  | otherwise
  = Pipe { start = (sender, typeRep)
         , cont  = procSend
         , env   = newEnv
         , end   = (receiver, typeRep)
         }
 where
  leftRecv     = startNat leftP
  rightRecv    = startNat rightP
  leftEndSend  = endNat leftP
  rightEndSend = endNat rightP

  isOptimized =
    (leftRecv == rightRecv)
      && (leftRecv == sender)
      && (leftEndSend == rightEndSend)
      && (leftEndSend == receiver)

  -- optmized zone

  -- did not cover the case where sub-sender == sub-receiver && not found in env
  -- did not cover the edge case where sender and receiver are the same

  -- unoptimized zone
  procSend =
    toAProcRTFunc (\x -> send' leftRecv (fl x) >> send' rightRecv (fr x))

  leftEnv =
    updateEnvWithSendProcSafe receiver leftEndSend
      $ updateEnvWithRecvProc sender leftP
  rightEnv =
    updateEnvWithSendProcSafe receiver rightEndSend
      $ updateEnvWithRecvProc sender rightP

  recvProc = toAProc $ do
    x :: Core c  <- recv' leftEndSend
    y :: Core c' <- recv' rightEndSend
    return (Pair x y)

  newEnv = Map.insertWith (flip bind) receiver recvProc
    $ Map.unionWith bind leftEnv rightEnv

selectWith
  :: Serialise e
  => (Core c -> Core e)
  -> (Core d -> Core e)
  -> Pipe a c
  -> Pipe b d
  -> Nat
  -> Nat
  -> Pipe (Either a b) e
selectWith (fl :: Core c -> Core e) fr (leftP@Pipe{} :: Pipe a c) (rightP@Pipe{} :: Pipe
    b
    d) sender receiver
  = Pipe { start = (sender, typeRep)
         , cont  = procSend
         , env   = newEnv
         , end   = (receiver, typeRep)
         }
 where
  leftRecv  = startNat leftP
  rightRecv = startNat rightP

  allRole =
    receiver : (Set.toList $ Set.union (getAllRoles leftP) (getAllRoles rightP))
  procSend = toAProcRTFunc
    (\x -> selectMulti' allRole
                        (x :: Core (Either a b))
                        (ignoreOutput . send' leftRecv)
                        (ignoreOutput . send' rightRecv)
    )

  leftEnv =
    updateEnvWithSendProcWith fl receiver (endNat leftP)
      $ updateEnvWithRecvProc sender leftP
  rightEnv =
    updateEnvWithSendProcWith fr receiver (endNat rightP)
      $ updateEnvWithRecvProc sender rightP

  branchProc = toAProc $ branchCont' sender
                                     ((recv' (endNat leftP)) :: ProcRT e)
                                     ((recv' (endNat rightP)) :: ProcRT e)

  -- procs that do both branches
  dupEnv = Map.intersectionWith (addBranch sender) leftEnv rightEnv
  -- procs that do action in the left branch, not in the right branch
  newLeftEnv =
    Map.map (\x -> addBranch sender x (toAProc $ return $ Lit ()))
      $ Map.difference leftEnv rightEnv
  -- procs that do action in the right branch, not in the left branch
  newRightEnv =
    Map.map (\x -> addBranch sender (toAProc $ return $ Lit ()) x)
      $ Map.difference rightEnv leftEnv

  -- if receiver already exists in the branch that means, receiver is the same as two end branches so ignore branchProc
  -- otherwise it's a bug
  -- since when the final receiver already exists in the one of the pipe => two branch action, one select => dead lock
  newEnv =
    Map.insertWith (flip const) receiver branchProc
      $ Map.union dupEnv
      $ Map.union newLeftEnv newRightEnv

generalMerger' :: Pipe b c -> Pipe b' c' -> Nat -> Nat -> Pipe (b, b') (c, c')
generalMerger' leftP@Pipe{} rightP@Pipe{} =
  mergeWith (Fst :$) (Snd :$) leftP rightP

mymerger' :: Pipe b c -> Pipe b c' -> Nat -> Nat -> Pipe b (c, c')
mymerger' leftP@Pipe{} rightP@Pipe{} = mergeWith id id leftP rightP

generalSelector'
  :: Pipe a c -> Pipe b d -> Nat -> Nat -> Pipe (Either a b) (Either c d)
generalSelector' leftP@Pipe{} rightP@Pipe{} =
  selectWith (Inl :$) (Inr :$) leftP rightP

selector' :: Pipe a c -> Pipe b c -> Nat -> Nat -> Pipe (Either a b) c
selector' leftP@Pipe{} rightP@Pipe{} = selectWith id id leftP rightP

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
getAProcRTofEndSafe Pipe {..} = Map.lookup (fst end) env

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

--   in fmap swap $ Map.toList $ Map.insertWith (>>) r (pre lk rk) $ helper True 1 0 1
forkJoinDc2
    :: Int
    -> Int
    -> Divider
    -> Combiner
    -> Basic
    -> (Nat -> Nat -> Core Int -> ProcRT ())
    -> (Nat -> Nat -> ProcRT Int)
    -> Pipe Int Int
forkJoinDc2 offset level dvd comb base pre post =
    let (_, lk, rk, r) = plrs 0
    in  Pipe
            { start = (startRole, typeRep :: TypeRep Int)
            , cont  = toAProcRTFunc $ pre lk rk
            , env   = Map.insert r (toAProc $ post lk rk)
                          $ Map.mapWithKey (const toAProc) (helper True 1 0 1)
            , end   = (startRole, typeRep :: TypeRep Int)
            }
  where
    startRole = toEnum offset
    helper :: Bool -> Int -> Int -> Int -> Map Nat (ProcRT ())
        -- reached the last node
    helper False _ _ curLvl | curLvl == 0 =
        let (_, _, _, r) = plrs 0
        in  Map.insertWith (>>) r (return (Lit ())) Map.empty
        -- reached a node in basic level
    helper True cur preLvl curLvl | curLvl == level =
        let roleInt        = getRoleInt preLvl cur
            (par, _, _, r) = plrs roleInt
        in  Map.insertWith
                (>>)
                r
                (base par par)
                (if cur /= 2 ^ curLvl
                    then helper True (cur + 1) preLvl curLvl
                    else helper False 1 (curLvl - 2) (curLvl - 1)
                )
        -- reached a node in the combine level
    helper False cur preLvl curLvl =
        let roleInt          = getRoleInt preLvl cur
            (par, lk, rk, r) = plrs roleInt
        in  Map.insertWith
                (>>)
                r
                (comb par lk rk)
                (if cur /= 2 ^ curLvl
                    then helper False (cur + 1) preLvl curLvl
                    else helper False 1 (curLvl - 2) (curLvl - 1)
                )
        -- reached the node in the dividing level
    helper True cur preLvl curLvl =
        let roleInt          = getRoleInt preLvl cur
            (par, lk, rk, r) = plrs roleInt
        in  Map.insertWith
                (>>)
                r
                (dvd par lk rk)
                (if cur /= 2 ^ curLvl
                    then helper True (cur + 1) preLvl curLvl
                    else helper True 1 curLvl (curLvl + 1)
                )

    plrs :: Int -> (Nat, Nat, Nat, Nat)
    plrs x =
        ( toEnum $ parent x + offset
        , toEnum $ leftKid x + offset
        , toEnum $ rightKid x + offset
        , toEnum $ x + offset
        )

    getRoleInt lvl cur = cur - 1 + totalNode lvl

    leftKid x = 2 * x + 1
    rightKid x = 2 * x + 2

    totalNode x = 2 ^ (x + 1) - 1

    parent 0 = 0
    parent x = ceiling (fromIntegral x / 2 :: Double) - 1