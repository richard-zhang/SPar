{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeOperators            #-}
module ParPattern where

import           Control.Monad
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Tuple
import           Data.Kind
import           Data.Type.Natural
import           Type.Reflection
import           Data.Maybe

import           Language.Poly.Core2
import           Language.Poly.Nat
import           RtDef
import           CodeGen.Type
import           CodeGen.Data
import           TypeValue

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 >>>, <<<

data Pipe a (b  :: Type) =
  (Serialise a, Serialise b) =>
  Pipe
    { start :: (Nat, SingleType a)
    , cont  :: AProcRTFunc a
    , env   :: Map Nat AProcRT
    , end   :: (Nat, SingleType b)
    }

type ArrowPipe a b = Nat -> Pipe a b

swapAway
  :: (Serialise a, Serialise b)
  => Proxy a
  -> Proxy b
  -> SNat n
  -> Core ((a, a) -> b)
  -> ArrowPipe (Tree ( 'S n) a) (Tree n b)
swapAway proxy proxy2 n func startRole = case (proofA, proofB, proofC) of
  (SDict, SDict, SDict) -> Pipe { start = (startRole, singleType)
                         , cont  = sender
                         , env   = myEnv
                         , end   = (endRole, singleType)
                         }
 where
  sender = case proofA of
    SDict -> toAProcRTFunc $ sendHelper proxy n (startRole + 1)
  endRole   = toEnum $ (fromEnum (startRole + 1)) + totalNode

  totalNode = (2 ^ (sNatToInt n :: Integer) :: Int)

  recvProc  = case proofB of
    SDict -> toAProc $ receiveHelper proxy2 n (startRole + 1)
  (proofA, proofB, proofC) = (getSDict (SS n) proxy, getSDict n proxy2, getSDict n proxy2)

  mkProc           = toAProc $ do
    x <- recv' startRole
    _ <- send' endRole (func :$ x)
    return (Lit ())
  roles =
    fmap toEnum
      $ [fromEnum $ startRole + 1 .. (fromEnum (startRole + 1)) + totalNode - 1]

  myEnv =
    Map.insert endRole recvProc $ Map.fromAscList $ zip roles (repeat mkProc)

receiveHelper :: (Serialise a) => Proxy a -> SNat n -> Nat -> ProcRT (Tree n a)
receiveHelper _ SZ startRole = do
  x <- recv' startRole
  return x
receiveHelper a (SS n) startRole = do
  x <- receiveHelper a n startRole
  y <- receiveHelper a n (toEnum startRoleRight)
  return (Pair x y)
  where startRoleRight = (fromEnum startRole) + (2 ^ sNatToInt n)

sendHelper
  :: (Serialise a)
  => Proxy a
  -> SNat n
  -> Nat
  -> Core (Tree n a, Tree n a)
  -> ProcRT ()
sendHelper _ SZ startRole x = do
  _ <- send' startRole x
  return (Lit ())
sendHelper a n startRole x = foldl1 (>>) sendAction
 where
  leftNodes = case getSDict n a of
    SDict -> getNodesCore a n (Fst :$ x)
  rightNodes = case getSDict n a of
    SDict -> getNodesCore a n (Snd :$ x)
  pairNodes   = zipWith Pair leftNodes rightNodes
  senderNodes = zip [fromEnum startRole ..] pairNodes
  sendAction  = fmap
    (\(role, val) -> send' (toEnum role) val >> return (Lit ()))
    senderNodes

pmap
  :: (Serialise a, Serialise b)
  => ArrowPipe a b
  -> ArrowPipe a (a, (a, (a, a)))
  -> ArrowPipe (b, (b, (b, b))) b
  -> ArrowPipe a b
pmap f s c = s >>> (f *** (f *** (f *** f))) >>> c

preduce :: (Serialise a) => ArrowPipe (a, a) a -> ArrowPipe (a, (a, (a, a))) a
preduce r = helper >>> (r *** r) >>> r
  where helper = (arr Id *** arr Fst) &&& (arr Snd >>> arr Snd)

paraMap
  :: (Serialise a, Serialise b)
  => ArrowPipe a b
  -> ArrowPipe (a, (a, (a, a))) (b, (b, (b, b)))
paraMap f = f *** f *** f *** f

-- hylomorphism
divConq
  :: (Serialise a, Serialise b, Serialise c)
  => ArrowPipe a b
  -> ArrowPipe a (Either c (a, a))
  -> ArrowPipe (Either c (b, b)) b
  -> Int
  -> ArrowPipe a b
divConq baseFunc _ _ 0 = baseFunc
divConq baseFunc alg coalg x =
  alg
    >>> (   arr Inl
        ||| (   (   (arr Fst >>> divConq baseFunc alg coalg (x - 1))
                &&& (arr Snd >>> divConq baseFunc alg coalg (x - 1))
                )
            >>> arr Inr
            )
        )
    >>> coalg

divConquer
  :: (Serialise a, Serialise b)
  => ArrowPipe a b
  -> ArrowPipe a (a, a)
  -> ArrowPipe (b, b) b
  -> Int
  -> ArrowPipe a b
divConquer baseFunc _split _merge 0 = baseFunc
divConquer baseFunc split merge level =
  split
    >>> (   divConquer baseFunc split merge (level - 1)
        *** divConquer baseFunc split merge (level - 1)
        )
    >>> merge

arrowId :: Serialise a => ArrowPipe a a
arrowId = arr Id

-- >>>
-- [r0 --- r1] >>> [r1 ---- ..]
(>>>) :: (ArrowPipe a b) -> (ArrowPipe b c) -> (ArrowPipe a c)
(>>>) leftArrow rightArrow sender = compose leftP rightP
 where
  leftP  = leftArrow sender
  -- continue with the last node in the lef branch
  rightP = rightArrow (endNat leftP)

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
  _   -> withOutNewRole
 where
  withOutNewRole = Pipe { start = (sender, singleType :: SingleType a)
                        , cont  = toAProcRTFunc (return . (f :$))
                        , env   = Map.empty
                        , end   = (sender, singleType :: SingleType b)
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
  leftP      = leftArrow rightStart
  rightStart = (endNat rightP) + 1
  rightP     = rightArrow sender

-- (&&&) :: a b c -> a b c' -> a b (c, c')
(&&&) :: (ArrowPipe b c) -> (ArrowPipe b c') -> ArrowPipe b (c, c')
-- (&&&) leftArrow rightArrow sender receiver = undefined
(&&&) leftArrow rightArrow sender = andHelper leftP rightP sender
 where
  leftP      = leftArrow rightStart
  rightStart = (endNat rightP) + 1
  rightP     = rightArrow sender

-- (+++)
-- notice the difference is we start with the right one
-- rightStart = sender  (not sneder + 1)
-- leftStart = rightMax + 1
(+++)
  :: (ArrowPipe a c) -> (ArrowPipe b d) -> ArrowPipe (Either a b) (Either c d)
(+++) leftArrow rightArrow sender = plusHelper leftP rightP sender
 where
  leftP  = leftArrow sender
  rightP = rightArrow sender
  -- rightP = rightArrow (sender + 1)
  -- leftP  = leftArrow (1 + getMaximum rightP)

-- (|||)
(|||) :: (ArrowPipe a c) -> (ArrowPipe b c) -> ArrowPipe (Either a b) c
(|||) leftArrow rightArrow sender = barHelper leftP rightP sender
 where
  leftP  = leftArrow sender
  rightP = rightArrow sender

arrowLeft :: Serialise d => ArrowPipe b c -> ArrowPipe (Either b d) (Either c d)
arrowLeft = (+++ arrowId)

arrowRight
  :: Serialise d => ArrowPipe b c -> ArrowPipe (Either d b) (Either d c)
arrowRight = (arrowId +++)

-- execution
localTypes :: [AProcessRT] -> [(Int, STypeV ())]
localTypes = map helper
  where
    helper (AProcRT _ value, role) = (fromEnum role, toSTypeV value)

prettyType :: ArrowPipe a b -> [(Int, String)]
prettyType = fmap (fmap prettyStype) . runType

runType :: ArrowPipe a b -> [(Int, STypeV ())]
runType = localTypes . fst . runPipe1 zero 

runPipe :: Nat -> Core a -> (ArrowPipe a b) -> [AProcessRT]
runPipe start x arrowPipe = runPipe' (arrowPipe start) x
 where
  runPipe' pipe val = toAProcesses $ Map.insertWith bind key procVal (env pipe)
   where
    key      = startNat pipe
    procFunc = cont pipe
    procVal  = callAProcRTFunc procFunc val
    toAProcesses procMap = fmap swap $ Map.toList procMap

-- higher-order execution
runPipe1 :: Nat -> ArrowPipe a b -> ([AProcessRT], EntryRole a b)
runPipe1 role arrowPipe =
  ( toAProcesses env
  , EntryRole { entryRole = role
              , startRole = startRole
              , endRole   = endRole
              , startType = getStartType pipe
              , endType   = getEndType pipe
              }
  )
 where
  startRole = role + 1
  endRole   = endNat pipe
  pipe      = arrowPipe startRole
  env =
    updateEnvWithSendProcSafe role endRole $ updateEnvWithRecvProc role pipe
  toAProcesses procMap = fmap swap $ Map.toList procMap

-- helper function zone
compose :: Pipe a b -> Pipe b c -> Pipe a c
compose first@Pipe{} second@Pipe{}
  | sender == receiver
  = helper eitherProcOrFunc
  | otherwise
  = error
    $  "compose "
    ++ (show $ fromEnum sender)
    ++ " "
    ++ (show $ fromEnum receiver)
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
        $ let newProc = proc `bind4Force` (cont second)
          in  maybe newProc (newProc `bind`) $ Map.lookup sender $ env second
    Nothing -> if startNat first == endNat first
      then Right $ (cont first) `bind3Force` (cont second)
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
  = tmpPipe
 where
  receiver     = leftEndSend
  rightRecv    = startNat rightP
  leftRecv     = startNat leftP
  leftEndSend  = endNat leftP
  rightEndSend = endNat rightP

  tmpPipe      = Pipe { start = (sender, singleType :: SingleType e)
                      , cont  = procSend
                      , env   = newEnv
                      , end   = (receiver, singleType :: SingleType (c, c'))
                      }

  (procFunc, newRightEnv) = extractRecvProc rightP
  updateProcFunc          = if rightEndSend == rightRecv
    then addSendFunc procFunc receiver
    else procFunc

  procSend =
    (toAProcRTFunc (\x -> send' leftRecv (fl x) >> return (fr x)))
      `bind3` updateProcFunc

  leftEnv =
    Map.update (\x -> Just $ x `bind4` recvProc) receiver
      $ updateEnvWithRecvProc sender leftP
  rightEnv = updateEnvWithSendProcSafe receiver rightEndSend $ newRightEnv

  recvProc = toAProcRTFunc $ \x -> do
    x'           <- forcedEval' x
    y :: Core c' <- recv' rightEndSend
    return ((Pair x' y) :: Core (c, c'))

  newEnv = Map.unionWith bind leftEnv rightEnv

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
  | leftRecv == sender && rightRecv == sender
  = Pipe { start = (sender, singleType)
         , cont  = procSend
         , env   = newEnv
         , end   = (receiver, singleType)
         }
  | otherwise
  = error "arrowSum"
 where
  leftRecv     = startNat leftP
  rightRecv    = startNat rightP
  leftEndSend  = endNat leftP
  rightEndSend = endNat rightP
  receiver     = maximum (leftEndSend, rightEndSend)

  allRole = Set.toList $ Set.delete sender $ Set.insert receiver $ Set.union
    (getAllRoles leftP)
    (getAllRoles rightP)

  (leftCont , oldLeftEnv ) = extractRecvProc leftP
  (rightCont, oldRightEnv) = extractRecvProc rightP

  updateLeftCont           = if leftEndSend == leftRecv
    then addSendWithFunc fl leftCont receiver leftRecv
    else leftCont

  updateRightCont = if rightEndSend == rightRecv
    then addSendWithFunc fr rightCont receiver leftRecv
    else rightCont

  procSend = bind5 allRole updateLeftCont updateRightCont

  leftEnv  = if leftEndSend /= leftRecv
    then updateEnvWithSendProcWith fl receiver leftEndSend oldLeftEnv
    else oldLeftEnv
  rightEnv = if rightEndSend /= rightRecv
    then updateEnvWithSendProcWith fr receiver rightEndSend oldRightEnv
    else oldRightEnv

  -- procs that do both branches
  dupEnv = Map.intersectionWith (addBranch sender) leftEnv rightEnv
  -- procs that do action in the left branch, not in the right branch
  newLeftEnv =
    Map.mapWithKey
        (\key val -> if key == receiver
          then addBranch sender val (toAProc $ (recv' rightEndSend :: ProcRT e))
          else addBranch sender val (toAProc $ return $ Lit ())
        )
      $ Map.difference leftEnv rightEnv
  -- procs that do action in the right branch, not in the left branch
  newRightEnv =
    Map.mapWithKey
        (\key val -> if key == receiver
          then addBranch sender (toAProc $ (recv' leftEndSend :: ProcRT e)) val
          else addBranch sender (toAProc $ return $ Lit ()) val
        )
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

addSendWithFunc
  :: (Serialise a, Serialise b)
  => (Core a -> Core b)
  -> AProcRTFunc d
  -> Nat
  -> Nat
  -> AProcRTFunc d
addSendWithFunc (f :: Core a -> Core b) (AProcRTFunc (ty :: TypeRep c) procFunc) receiver sender
  = case ty `eqTypeRep` rep of
    Just HRefl -> if receiver /= sender
      then toAProcRTFunc (procFunc >=> \x -> send' receiver (f x))
      else toAProcRTFunc (procFunc >=> (return . f))
    Nothing -> error "typed mismatched in addSend"
  where rep = typeRep :: TypeRep a

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

extractRecvProc :: Pipe a b -> (AProcRTFunc a, Map Nat AProcRT)
extractRecvProc p@Pipe {..} = (newAProcRTFunc, newEnv)
 where
  (maybeProc, newEnv) =
    Map.updateLookupWithKey (\_ _ -> Nothing) (startNat p) env
  newAProcRTFunc = case maybeProc of
    Just proc -> cont `bind2` proc
    Nothing   -> cont

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
updateEnvWithSendProcWith (f :: Core a -> Core b) receiver sender env
  | receiver /= sender = Map.insert sender
                                    (getSendProcWith f receiver sender env)
                                    env
  | otherwise = Map.update helper receiver env
 where
  helper (AProcRT ty proc) = case ty `eqTypeRep` (typeRep :: TypeRep a) of
    Just HRefl -> Just $ toAProc (proc >>= (return . f))
    Nothing    -> error "type not compatible in updateEnvWithSend"

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

bind3Force :: AProcRTFunc a -> AProcRTFunc b -> AProcRTFunc a
bind3Force (AProcRTFunc ty func) (AProcRTFunc ty2 (func2 :: Core c -> ProcRT b))
  = case ty `eqTypeRep` rep of
    Just HRefl -> AProcRTFunc ty2 (func >=> forcedEval' >=> func2)
    Nothing    -> error "Two AProcRTFunc are not compatible"
  where rep = typeRep :: TypeRep c

bind4 :: AProcRT -> AProcRTFunc a -> AProcRT
bind4 (AProcRT ty proc) (AProcRTFunc ty2 (func :: Core c -> ProcRT b)) =
  case ty `eqTypeRep` rep of
    Just HRefl -> AProcRT ty2 (proc >>= func)
    Nothing    -> error "AProcRT and AProcRTFunc are not compatible"
  where rep = typeRep :: TypeRep c

bind4Force :: AProcRT -> AProcRTFunc a -> AProcRT
bind4Force (AProcRT ty proc) (AProcRTFunc ty2 (func :: Core c -> ProcRT b)) =
  case ty `eqTypeRep` rep of
    Just HRefl -> AProcRT ty2 (proc >>= forcedEval' >>= func)
    Nothing    -> error $ show ty ++ " " ++ show rep ++ " AProcRT and AProcRTFunc are not compatible"
  where rep = typeRep :: TypeRep c

bind5 :: [Nat] -> AProcRTFunc a -> AProcRTFunc b -> AProcRTFunc (Either a b)
bind5 allRole (AProcRTFunc tyl fl) (AProcRTFunc tyr fr) =
  case tyl `eqTypeRep` tyr of
    Just HRefl -> toAProcRTFunc (\x -> selectMulti' allRole x fl fr)
    Nothing    -> toAProcRTFunc
      (\x -> selectMulti' allRole x (ignoreOutput . fl) (ignoreOutput . fr))

getRecvProcOrFunc :: Pipe a b -> Nat -> Either AProcRT (AProcRTFunc a)
getRecvProcOrFunc pipe@Pipe {..} receiver =
  case getAProcRTSafe pipe receiver of
    Just proc -> Left proc
    Nothing   -> if startNat pipe == endNat pipe
      then Right cont
      else error "mistake can't find"

nextRole :: Nat -> Nat
nextRole x = x + 1

getStartType :: Pipe a b -> SingleType a
getStartType Pipe {..} = snd start

getEndType :: Pipe a b -> SingleType b
getEndType Pipe {..} = snd end
