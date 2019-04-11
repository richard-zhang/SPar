{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}

module Pattern where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple
import Data.Type.Natural
import Type.Reflection

import Language.Poly.Core
import RtDef

type GrainSize = Int

-- recv from the 1, split (usually) and send left part to 2, right part to thrid to 3
type Divider = Nat -> Nat -> Nat -> ProcRT ()

-- recv left from 2, right from 3, combine and send to 1
type Combiner = Nat -> Nat -> Nat -> ProcRT ()

-- recv from 1, send to 2
type Basic = Nat -> Nat -> ProcRT ()

-- thinking
type Splitter = [Nat] -> ProcRT ()

type Merger = [Nat] -> ProcRT ()

-- fst is input, snd is output, third is maximum node used fourth is list of process associated
type Node1 = (Nat, Nat, Map Nat (ProcRT ()))

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

data Pipe a b = Pipe
  { start :: (Nat, TypeRep a)
  , cont  :: AProcRTFunc a
  , env   :: Map Nat AProcRT
  , end   :: (Nat, TypeRep b)
  }

callAProcRTFunc :: AProcRTFunc a -> Core a -> AProcRT
callAProcRTFunc (AProcRTFunc ty cont) val = AProcRT ty (cont val)

getAProcRTofEnd :: Pipe a b -> AProcRT
getAProcRTofEnd Pipe {..} = fromMaybe (error "no procRT for end") (Map.lookup (fst end) env)

addRecv :: AProcRTFunc a -> Nat -> AProcRT
addRecv ((AProcRTFunc ty cont) :: AProcRTFunc a) sender = AProcRT ty wholeProc
  where
    rep = typeRep :: TypeRep a
    recvProc = recv' sender :: ProcRT a
    wholeProc = recvProc >>= cont

addSend :: AProcRT -> Nat -> AProcRT
addSend (AProcRT (ty :: TypeRep a) proc) receiver = AProcRT ty (proc >>= cont)
  where
    cont = send' receiver :: Core a -> ProcRT a

compose :: Pipe a b -> Pipe b c -> Pipe a c
compose first second = Pipe {start = start first, cont = cont first, env = newEnv, end = end second}
  where
    procSend = addSend (getAProcRTofEnd first) receiver
    procRecv = addRecv combinedProcFunc sender
    combinedProcFunc = case Map.lookup receiver (env second) of
      Just proc -> cont second `bind2` proc
      Nothing   -> cont second
    sender = fst $ end first
    receiver = fst $ start second
    newEnv = Map.insert receiver procRecv $ Map.insert sender procSend $ Map.union (env first) (env second)

bind :: AProcRT -> AProcRT -> AProcRT
bind (AProcRT ty1 proc1) (AProcRT ty2 proc2) = AProcRT ty2 (proc1 >> proc2)

bind2 :: AProcRTFunc a -> AProcRT -> AProcRTFunc a
bind2 (AProcRTFunc ty func) (AProcRT ty2 proc) = AProcRTFunc ty2 ((>> proc) . func)

runPipe :: Pipe a b -> Core a -> [ProcessRT ()]
runPipe pipe val = toProcessRT $ Map.insertWith bind key procVal (env pipe)
  where
    key = fst $ start pipe
    procFunc = cont pipe
    procVal = callAProcRTFunc procFunc val

    toProcessRT map = let f (AProcRT ty proc) = ignoreOutput proc
      in swap . fmap f <$> Map.toList map

toAProc :: Serialise a => ProcRT a -> AProcRT
toAProc (val :: ProcRT a) = AProcRT (typeRep :: TypeRep a) val

toAProcRTFunc :: (Serialise a, Serialise b) => (Core a -> ProcRT b) -> AProcRTFunc a
toAProcRTFunc (f :: Core a -> ProcRT b) = AProcRTFunc (typeRep :: TypeRep b) f

helloWorld3 = runPipe (compose (helloWorld2' 0 1) (helloWorld2' 10 1)) (Lit 10 :: Core Int)
-- helloWorld3 = runPipe (helloWorld2' 10 1) (Lit 10 :: Core Int)

helloWorld2' :: Int -> Int -> Pipe Int Int
helloWorld2' x y = forkJoinDc2 x y div comb basic pre post
  where
    div source left right = do
      x :: Core Int <- recv' source
      send' left x
      send' right x
      return (Lit ())
    comb target left right = do
      x :: Core Int <- recv' left
      _y :: Core Int <- recv' right
      send' target x
      return (Lit ())
    basic source target = do
      x :: Core Int <- recv' source
      send' target x
      return (Lit ())
    pre left right = \x -> do
      send' left x
      send' right x
      return (Lit ())
    post left right = do
      x :: Core Int <- recv' left
      y :: Core Int <- recv' right
      return x

forkJoinDc2 ::
     Int
  -> Int
  -> Divider
  -> Combiner
  -> Basic
  -> (Nat -> Nat -> Core Int -> ProcRT ())
  -> (Nat -> Nat -> ProcRT Int)
  -> Pipe Int Int
forkJoinDc2 offset level div comb base pre post =
  let (_, lk, rk, r) = plrs offset 0
--   in fmap swap $ Map.toList $ Map.insertWith (>>) r (pre lk rk) $ helper True 1 0 1
     in Pipe {start = (startRole, typeRep :: TypeRep Int),
              cont = toAProcRTFunc $ pre lk rk,
              env = Map.insert r (toAProc $ post lk rk) $ Map.mapWithKey (const toAProc) (helper True 1 0 1),
              end = (startRole, typeRep :: TypeRep Int) }
--   in fmap swap $ Map.toList $ helper True 1 0 1
        -- helper isDiv cur preLevel curLevel
  where
    startRole = toEnum offset
    helper :: Bool -> Int -> Int -> Int -> Map Nat (ProcRT ())
        -- reached the last node
    helper False _ _ curLvl
      | curLvl == 0 =
        let (_, _, _, r) = plrs offset 0
         in Map.insertWith (>>) r (return (Lit ())) Map.empty
        -- reached a node in basic level
    helper True cur preLvl curLvl
      | curLvl == level =
        let roleInt = getRoleInt preLvl cur
            (par, _, _, r) = plrs offset roleInt
         in Map.insertWith
              (>>)
              r
              (base par par)
              (if cur /= 2 ^ curLvl
                 then helper True (cur + 1) preLvl curLvl
                 else helper False 1 (curLvl - 2) (curLvl - 1))
        -- reached a node in the combine level
    helper False cur preLvl curLvl =
      let roleInt = getRoleInt preLvl cur
          (par, lk, rk, r) = plrs offset roleInt
       in Map.insertWith
            (>>)
            r
            (comb par lk rk)
            (if cur /= 2 ^ curLvl
               then helper False (cur + 1) preLvl curLvl
               else helper False 1 (curLvl - 2) (curLvl - 1))
        -- reached the node in the dividing level
    helper True cur preLvl curLvl =
      let roleInt = getRoleInt preLvl cur
          (par, lk, rk, r) = plrs offset roleInt
       in Map.insertWith
            (>>)
            r
            (div par lk rk)
            (if cur /= 2 ^ curLvl
               then helper True (cur + 1) preLvl curLvl
               else helper True 1 curLvl (curLvl + 1))
-- splitToN easy version
-- split a computation by chunk of n, and for computation of each chunk, use forkJoin parallel pattern
--splitToN :: (Nat -> ProcRT ()) -> (Nat -> ProcRT ()) (Nat -> Nat -> ProcRT ()) -> (Nat -> Nat -> ProcRT ()) -> Divider -> Combiner -> Basic -> Splitter -> Merger -> Int -> Int -> Node
--splitToN receiver sender pre post div comb base splitter merger n start = undefined
--  where
--
--    helper x = forkJoinNode x 2 div comb base pre post
--
--    helper2 x | x /= n = let (_, _, maxi, procs) = helper x; (receivers, mergeProcs) = helper2 (fromEnum maxi) in (x : receivers, procs ++ mergeProcs)
--              | otherwise = let (_, _, _, procs) = helper x in  ([x], procs)

-- selector :: Node1 -> Node1 -> (Nat -> Nat -> Nat -> ProcRT ()) -> Nat -> Node1
-- selector left@(sl, fl, procl) right@(sr, fr, procr) select start
--   | fl == fr = (start, fl, finalProcs)
--   where
--     selecProc = select start sl sr
--     leftProc = fromJust $ Map.lookup sl procl
--     rightProc = fromJust $ Map.lookup sr procr
--     leftM = branch' start leftProc (return (Lit ()))
--     rightM = branch' start (return (Lit ())) rightProc
--     finalProcs =
--       (Map.insert start selecProc . Map.update (const (Just leftM)) sl . Map.update (const (Just rightM)) sr)
--         (Map.union procl procr)

helloWorld :: [(ProcRT (), Nat)]
helloWorld = forkJoinDc 0 2 div comb basic pre post
  where
    div source left right = do
      x :: Core Int <- recv' source
      send' left x
      send' right x
      return (Lit ())
    comb target left right = do
      x :: Core Int <- recv' left
      _y :: Core Int <- recv' right
      send' target x
      return (Lit ())
    basic source target = do
      x :: Core Int <- recv' source
      send' target x
      return (Lit ())
    pre left right = do
      send' left (Lit (10 :: Int))
      send' right (Lit (20 :: Int))
      return (Lit ())
    post left right = do
      x :: Core Int <- recv' left
      y :: Core Int <- recv' right
      return (Lit ())

-- -> [(ProcRT a, Nat)]
--- pre process contains information about the source data, the source data might be derivided from receving or local computation
forkJoinDc ::
     Int
  -> Int
  -> Divider
  -> Combiner
  -> Basic
  -> (Nat -> Nat -> ProcRT ())
  -> (Nat -> Nat -> ProcRT ())
  -> [(ProcRT (), Nat)]
forkJoinDc offset level div comb base pre post =
  let (_, lk, rk, r) = plrs offset 0
   in fmap swap $ Map.toList $ Map.insertWith (>>) r (pre lk rk) $ helper True 1 0 1
        -- helper isDiv cur preLevel curLevel
  where
    helper :: Bool -> Int -> Int -> Int -> Map Nat (ProcRT ())
        -- reached the last node
    helper False _ _ curLvl
      | curLvl == 0 =
        let (_, lk, rk, r) = plrs offset 0
         in Map.insertWith (>>) r (post lk rk) Map.empty
        -- reached a node in basic level
    helper True cur preLvl curLvl
      | curLvl == level =
        let roleInt = getRoleInt preLvl cur
            (par, _, _, r) = plrs offset roleInt
         in Map.insertWith
              (>>)
              r
              (base par par)
              (if cur /= 2 ^ curLvl
                 then helper True (cur + 1) preLvl curLvl
                 else helper False 1 (curLvl - 2) (curLvl - 1))
        -- reached a node in the combine level
    helper False cur preLvl curLvl =
      let roleInt = getRoleInt preLvl cur
          (par, lk, rk, r) = plrs offset roleInt
       in Map.insertWith
            (>>)
            r
            (comb par lk rk)
            (if cur /= 2 ^ curLvl
               then helper False (cur + 1) preLvl curLvl
               else helper False 1 (curLvl - 2) (curLvl - 1))
        -- reached the node in the dividing level
    helper True cur preLvl curLvl =
      let roleInt = getRoleInt preLvl cur
          (par, lk, rk, r) = plrs offset roleInt
       in Map.insertWith
            (>>)
            r
            (div par lk rk)
            (if cur /= 2 ^ curLvl
               then helper True (cur + 1) preLvl curLvl
               else helper True 1 curLvl (curLvl + 1))

getRoleInt :: Int -> Int -> Int
getRoleInt lvl cur = cur - 1 + totalNode lvl

totalNode :: Int -> Int
totalNode x = 2 ^ (x + 1) - 1

plrs :: Int -> Int -> (Nat, Nat, Nat, Nat)
plrs offset x =
  (toEnum $ parent x + offset, toEnum $ leftKid x + offset, toEnum $ rightKid x + offset, toEnum $ x + offset)

leftKid :: Int -> Int
leftKid x = 2 * x + 1

rightKid :: Int -> Int
rightKid x = 2 * x + 2

parent :: Int -> Int
parent 0 = 0
parent x = ceiling (fromIntegral x / 2) - 1
-- Question zone or thinking
-- add a field to represent starting int
-- you don't even care about the communication pattern
-- refactor into monad
-- One thread corresponds to one proc => cannot reused thread ?
-- For a node, it only cares about input roles and output roles and don't need to check data exchange has the corresponding type
-- Composability Haskell Talk
-- extract algebraic structure of pattern compositions
-- arity polymorphic function to support multiple way
-- use arrow type class to approximate ?
-- graph monad to avoid vetex duplication when composing graph?
-- use monad?
-- session type => progress guarantee
-- every inside the blackbox need to have a branch operation