{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
module Mergesort where
import           Data.Type.Natural

import           Language.Poly.Core
import           Language.Poly.Type
import           RtDef
import           Pattern

-- the version without recursive data type
-- K (() + int) + I * I 
-- Either (Either () Int) ([a], [a])
-- split :: Core [a] -> C
split :: Core ([a] -> (Either (Either () Int) ([a], [a])))
split = Prim "split" undefined

merge :: Core ((Either (Either () Int) ([a], [a])) -> [a])
merge = Prim "merge" undefined

sort :: Core ([a] -> [a])
sort = Prim "sort" undefined

mergeSortBase :: Serialise a => Nat -> Nat -> Pipe [a] [a]
mergeSortBase =
    arr split >>>
    (arr Inl
        |||
        (
        (arr Fst >>> arr sort)
           &&&
        (arr Snd >>> arr sort)
        >>>
        arr Inr
        )
    ) >>>
    arr merge

mergeSort :: Serialise a => Int -> Nat -> Nat -> Pipe [a] [a]
mergeSort 0 = mergeSortBase
mergeSort x =
    arr split >>>
    (arr Inl
        |||
        (
        (arr Fst >>> mergeSort (x-1)) 
           &&&
        (arr Snd >>> mergeSort (x-1))
        >>>
        arr Inr
        )
    ) >>>
    arr merge