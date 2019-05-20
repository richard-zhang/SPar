module Ms where
import Spar

main :: IO ()    
main = benchmarking "benchmark/mergesort" [100, 99, 101, 98, 103] [0..4] [4..20] mergeSort
-- main = codeGenDebug False testArr
-- main = codeGenDebug1 False testArr1
-- main = forM_ [0..2] (\r -> benchmarkList "benchmark/mergesort" 100 r 5 mergeSort >>= print)
-- main = benchmarkList "ct" 1 4 mergeSort >>= print