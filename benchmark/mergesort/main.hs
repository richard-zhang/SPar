module Ms where
import Spar

instance GenRange Int where
    lBound = minBound
    rBound = maxBound

main :: IO ()    
main = benchmarking "benchmark/mergesort" [100, 99, 101, 98, 103] [0..3] [0..20] mergeSort