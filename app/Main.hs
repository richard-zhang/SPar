module Main where
import Spar
import Control.Monad

main :: IO ()
-- main = codeGenDebug False testArr
-- main = codeGenDebug1 False testArr1
-- main = codeGenDebug1 False testWordCount
main = codeGenBench [1,2, 1, 2, 1, 2, 1, 2, 1,1,1,1,1,1,1,5,5,5,6,6,6,6,8,9,9,9] testWordCount "benchmark/intcount/test" >> return ()
-- main = codeGenBench (getList 100 4) testMergeSort "benchmark/mergesort/test" >> return ()
-- main = putStrLn "hello, world"
