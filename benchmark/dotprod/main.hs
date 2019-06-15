module Main where
import Spar

main :: IO ()
-- main = benchmarking "benchmark/dotprod" [100] [0..3] [15..20] dotProd
main = benchmarkEntry "benchmark/dotprod" 10 [1] [0..4] [15..24] dotProd