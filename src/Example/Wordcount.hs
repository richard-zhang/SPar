module Example.Wordcount where

import           Lib
import           ParPattern

testWordCount = runPipe1 zero expr
  where
    expr = wordCount 3
    val  = Lit [1, 2, 4, 3, 2, 1, 10, 1000, 50, 6, 100, 4, 5, 100] :: Core [Int]

union :: Core (([(Int, Int)], [(Int, Int)]) -> [(Int, Int)])
union = Prim "myunion" undefined

count :: Core ([Int] -> [(Int, Int)])
count = Prim "count" undefined

splitw :: Core ([Int] -> Either [Int] ([Int], [Int]))
splitw = Prim "split" undefined

wordCount2 :: Int -> ArrowPipe [Int] [(Int, Int)]
wordCount2 = divConq (arr count) (arr splitw) (arr count ||| arr union)

wordCount :: Int -> ArrowPipe [Int] [(Int, Int)]
wordCount 0 = arr count
wordCount x =
    arr splitw
        >>> (   arr Inl
            ||| (   (   (arr Fst >>> wordCount (x - 1))
                    &&& (arr Snd >>> wordCount (x - 1))
                    )
                >>> arr Inr
                )
            )
        >>> (arr count ||| arr union)
