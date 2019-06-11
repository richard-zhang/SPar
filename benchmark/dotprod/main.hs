module Main where
import Spar

-- instance GenRange Int where
--     lBound = minBound
--     rBound = maxBound

main :: IO ()
-- main = benchmarking "benchmark/dotprod" [100] [0..3] [15..20] dotProd
main = benchmarkEntry "benchmark/dotprod" 1 [1,2,3] [0..4] [15..22] dotProd