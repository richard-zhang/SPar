module Main where
import Spar

instance GenRange Int where
    lBound = -2^12
    rBound = 2^12

main :: IO ()
main = benchmarking "benchmark/dotprod" [100] [0..3] [15..20] dotProd