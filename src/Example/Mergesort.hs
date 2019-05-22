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

testMergeSort = runPipe1 zero (mergeSort 1)
  where
    val = Lit [1, 2, 4, 3, 2, 1, 10, 1000, 50, 6, 100, 4, 5, 100] :: Core [Int]

split :: Core ([Int] -> (Either (Either () Int) ([Int], [Int])))
split = Prim "split" undefined

merge :: Core ((Either (Either () Int) ([Int], [Int])) -> [Int])
merge = Prim "merge" undefined

sort :: Core ([Int] -> [Int])
sort = Prim "sort" undefined

mergeSort2 :: Int -> ArrowPipe [Int] [Int]
mergeSort2 = divConq (arr sort) (arr split) (arr merge)

mergeSort :: Int -> ArrowPipe [Int] [Int]
mergeSort 0 = arr sort
mergeSort x =
    arr split
        >>> (   arr Inl
            ||| (   (arr Fst >>> mergeSort (x - 1))
                &&& (arr Snd >>> mergeSort (x - 1))
                >>> arr Inr
                )
            )
        >>> arr merge
