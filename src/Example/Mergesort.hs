{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
module Example.Mergesort where
import           Lib
import ParPattern
-- import Pattern

testArr :: [ProcessRT ()]
testArr =
    -- runPipe one twenty val $ expr
    runPipe one val $ expr
 
func = Prim "test" undefined :: Core (Int -> Int)
func1 = Prim "test1" undefined :: Core ((Int, Int) -> (Int, Int))

expr = ((arr func) &&& (arr func)) >>> arr func1 >>> arr Fst >>> arr func
val = Lit (3) :: Core (Int)

-- func = Prim "test" undefined :: Core (Int -> Int)
-- func1 = Prim "test1" undefined :: Core ((Int, Int) -> (Int, Int))
-- 
-- expr = ((arr func) &&& (arr func)) >>> arr func1 >>> arr Fst >>> arr func
-- val = Lit (3) :: Core (Int)

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

-- mergeSortBase :: Serialise a => ArrowPipe [a] [a]
-- mergeSortBase =
--     arr split
--         >>> (   arr Inl
--             ||| (   (arr Fst >>> arr sort)
--                 &&& (arr Snd >>> arr sort)
--                 >>> arr Inr
--                 )
--             )
--         >>> arr merge

-- mergeSort 0 = mergeSortBase
-- mergeSort x =
--     arr split
--         >>> (   arr Inl
--             ||| (   (arr Fst >>> mergeSort (x - 1))
--                 &&& (arr Snd >>> mergeSort (x - 1))
--                 >>> arr Inr
--                 )
--             )
--         >>> arr merge

-- testMergeSort :: [ProcessRT ()]
-- testMergeSort =
--     -- runPipe mergeSortBase one two (Lit [1, 2, 4, 3, 2, 1] :: Core [Int])
--     runPipe mergeSortBase one (Lit [1, 2, 4, 3, 2, 1] :: Core [Int])
