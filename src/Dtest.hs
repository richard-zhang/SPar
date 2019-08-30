{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dtest where

import           Data.Type.Natural
import           Data.Type.Natural.Class.Order
import           Data.Type.Equality
import           Proof.Propositional
import           Data.Singletons.Prelude.Enum
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

import           Language.Poly.Core2
import           RtDef
import           ParPattern
import           CodeGen.Type

data SDict a where
    SDict :: Serialise a => SDict a

getSDict :: Serialise a => SNat n -> Proxy a -> SDict (Tree n a)
getSDict SZ     _ = SDict
getSDict (SS n) p = case getSDict n p of
    SDict -> SDict

type family Tree (n :: Nat) (a :: *) where
    Tree 'Z a = a
    Tree ('S n) a = (Tree n a, Tree n a)

-- swapAway
--   :: (Serialise a)
--   => Proxy a -> SINat n
--   -> ArrowPipe (a, a) a
--   -> ArrowPipe (Tree n a, Tree n a) (Tree n a)
-- swapAway a sing f startRole = undefined

-- getNodes :: (m <= n) ~ 'True => SNat m -> SNat n -> Tree n Int -> [Tree (n -m) Int]
-- getNodes SZ _ x = [x]
-- getNodes (a@(SS m) :: SNat m) (n :: SNat n) x = undefined
--     where
--         firstProof = succLeqToLT m n (Witness :: IsTrue (m <= n))
--         secondProof = ltToLeq m n firstProof
--         y = case secondProof of Witness -> getNodes m n x
swapAway
    :: (Serialise a)
    => Proxy a
    -> SNat n
    -> Core ((a, a) -> a)
    -> ArrowPipe (Tree ( 'S n) a) (Tree n a)
swapAway proxy n func startRole = case (proofA, proofB) of
    (SDict, SDict) -> Pipe { start = (startRole, singleType)
                           , cont  = sender
                           , env   = myEnv
                           , end   = (endRole, singleType)
                           }
  where
    sender = case proofA of
        SDict -> toAProcRTFunc $ sendHelper proxy n (startRole + 1)
    endRole  = toEnum $ (fromEnum (startRole + 1)) + totalNode + 1

    totalNode = (2 ^ (sNatToInt n :: Integer) :: Int)

    recvProc = case proofB of
        SDict -> toAProc $ receiveHelper proxy n (startRole + 1)
    (proofA, proofB) = (getSDict (SS n) proxy, getSDict n proxy)

    mkProc           = toAProc $ do
        x <- recv' startRole
        _ <- send' endRole (func :$ x)
        return (Lit ())
    roles =
        fmap toEnum
            $ [fromEnum $ startRole + 1 .. (fromEnum (startRole + 1))
              + totalNode]

    myEnv = Map.insert endRole recvProc $ Map.fromAscList $ zip
        roles
        (repeat mkProc)


receiveHelper :: (Serialise a) => Proxy a -> SNat n -> Nat -> ProcRT (Tree n a)
receiveHelper _ SZ startRole = do
    x <- recv' startRole
    return x
receiveHelper a (SS n) startRole = do
    x <- receiveHelper a n startRole
    y <- receiveHelper a n (toEnum startRoleRight)
    return (Pair x y)
    where startRoleRight = (fromEnum startRole) + (sNatToInt n)

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

getNodesCore
    :: (Serialise a) => Proxy a -> SNat n -> Core (Tree n a) -> [Core a]
getNodesCore proxy n x = case minusNilpotent n of
    Refl -> getLevelsCore (leqRefl n) proxy n n x

getLevelsCore
    :: (Serialise a)
    => IsTrue (m <= n)
    -> Proxy a
    -> SNat m
    -> SNat n
    -> Core (Tree n a)
    -> [Core (Tree (n - m) a)]
getLevelsCore _       _     SZ     _ x = [x]
getLevelsCore witness proxy (SS m) n x = concat
    $ fmap (getterCore firstProof proxy m n) y
  where
    firstProof     = succLeqToLT m n witness
    secondWitnesss = ltToLeq m n firstProof
    y              = getLevelsCore secondWitnesss proxy m n x

getterCore
    :: (Serialise a)
    => Compare m n :~: 'LT
    -> Proxy a
    -> SNat m
    -> SNat n
    -> Core (Tree (n - m) a)
    -> [Core (Tree (n - ( 'S m)) a)]
getterCore comp proxy m n x = case pred2 comp m n of
    Refl -> helper gtZeroProof proxy snum x
  where
    snum        = sMinus comp n m
    gtZeroProof = gtZero comp m n

    helper
        :: (Serialise a)
        => IsTrue (Zero < n)
        -> Proxy a
        -> SNat n
        -> Core (Tree n a)
        -> [Core (Tree (Pred n) a)]
    helper witness proxy a@(SS n1) x = case getSDict n1 proxy of
        SDict -> [Fst :$ x, Snd :$ x]

getNodes
    :: IsTrue (m <= n) -> SNat m -> SNat n -> Tree n Int -> [Tree (n - m) Int]
getNodes _       SZ     _ x = [x]
getNodes witness (SS m) n x = concat $ fmap (getter firstProof m n) y
  where
    firstProof     = succLeqToLT m n witness
    secondWitnesss = ltToLeq m n firstProof
    y              = getNodes secondWitnesss m n x

getter
    :: Compare m n :~: 'LT
    -> SNat m
    -> SNat n
    -> Tree (n - m) Int
    -> [Tree (n - ( 'S m)) Int]
getter comp m n x = case pred2 comp m n of
    Refl -> helper gtZeroProof snum x
  where
    snum        = sMinus comp n m
    gtZeroProof = gtZero comp m n

    helper :: IsTrue (Zero < n) -> SNat n -> Tree n Int -> [Tree (Pred n) Int]
    helper witness a@(SS n1) x = [fst x, snd x]

gtZero :: Compare m n :~: 'LT -> SNat m -> SNat n -> IsTrue (Zero < (n - m))
gtZero = undefined

pred2
    :: Compare m n :~: 'LT
    -> SNat m
    -> SNat n
    -> (Pred (n - m)) :~: (n - ( 'S m))
pred2 = undefined

sMinus :: Compare m n :~: 'LT -> SNat n -> SNat m -> SNat (n - m)
sMinus comp n      SZ     = n
sMinus comp (SS n) (SS m) = sMinus b n m
  where
    a = cmpSucc m n
    b = a `trans` comp
sMinus comp SZ _ = error "not gonna happen"
