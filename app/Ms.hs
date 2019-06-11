module Ms where
import Spar

main :: IO ()
-- main = codeGenBenchCompile ([1..16], [1..16]) testNewDotProd "benchmark/newdotprod/test"
main = codeGenBenchCompile [(0, 0), (1, 1), (1, 0), (2, 2), (4, 4), (0, 3), (1, 2), (3, 1), (3, 3)] testQuickHull "benchmark/quickhull/test"
-- main = codeGenDebug1 False (testQuickHull) 
-- main :: IO ()    
-- main = benchmarking "benchmark/mergesort" [100, 99, 101, 98, 103] [0..6] [0..5] mergeSort
-- main = benchmarking "benchmark/intcount" [100, 99, 101, 98, 103] [0..6] [0..5] wordCount
-- main = benchmarking "benchmark/dotprod" [100] [0..4] [0..3] dotProd