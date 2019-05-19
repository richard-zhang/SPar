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
import           ParPattern

testArr = runPipe one val $ expr
-- testArr1 = runPipe1 zero (arr func)
testArr1 = runPipe1 zero expr

func = Prim "test" undefined :: Core (Int -> (Int, Int))
func1 = Prim "test1" undefined :: Core ((Int, Int) -> (Int, Int))
func3 = Prim "test3" undefined :: Core (Int -> Either Int Int)
func4 str = Prim str undefined :: Core (Int -> Int)
func5 = Prim "test5" undefined :: Core (Int -> ())
func6 = Prim "test6" undefined :: Core (a -> a)

-- expr = ((arr func) &&& (arr func)) >>> arr func1 >>> arr Fst >>> arr func
expr = mergeSort 0
val = Lit [1, 2, 4, 3, 2, 1, 10, 1000, 50, 6, 100, 4, 5, 100] :: Core [Int]

split :: Core ([Int] -> (Either (Either () Int) ([Int], [Int])))
split = Prim "split" undefined

merge :: Core ((Either (Either () Int) ([Int], [Int])) -> [Int])
merge = Prim "merge" undefined

sort :: Core ([Int] -> [Int])
sort = Prim "sort" undefined

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