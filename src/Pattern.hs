{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}

module Pattern where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple
import Data.Type.Natural
import Data.Maybe

import RtDef
import Language.Poly.Core

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
type Node = (Nat, Nat, Map Nat (ProcRT ()))

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

forkJoinNode :: Int -> Int -> Divider -> Combiner -> Basic -> (Nat -> Nat -> ProcRT ()) -> (Nat -> Nat -> ProcRT ()) -> Node
forkJoinNode offset level div comb base pre post = undefined

selector :: Node -> Node -> (Nat -> Nat -> Nat -> ProcRT ()) -> Nat -> Node
selector left@(sl, fl, procl) right@(sr, fr, procr) select start | fl == fr =
  (start, fl, finalProcs)
  where
    selecProc = select start sl sr
    leftProc = fromJust $ Map.lookup sl procl
    rightProc = fromJust $ Map.lookup sr procr

    leftM = branch' start leftProc (return (Lit ()))
    rightM = branch' start (return (Lit ())) rightProc

    finalProcs = (Map.insert start selecProc . Map.update (const (Just leftM)) sl. Map.update (const (Just rightM)) sr) (Map.union procl procr)


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
  where
        -- helper isDiv cur preLevel curLevel
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
totalNode x = 2^(x + 1) - 1

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
