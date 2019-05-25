module Main where
import Spar
import Control.Monad

main :: IO ()
-- main = codeGenDebug1 False testMergeSort
-- main = codeGenDebug1 False testWordCount
-- main = codeGenDebug1 False testDotProd
main = codeGenBenchCompile [1,2, 1, 2, 1, 2, 1, 2, 1,1,1,1,1,1,1,5,5,5,6,6,6,6,8,9,9,9] testWordCount "benchmark/intcount/test" 
-- main = codeGenBenchCompile(let size = 2^20 in (take size [1,1..], take size [1,1..])) testDotProd "benchmark/dotprod/test" 
-- main = codeGenBenchCompile[5,4,3,2,1,2,3,4,5,6,6,7] testMergeSort "benchmark/mergesort/test"
-- main = putStrLn "hello, world"