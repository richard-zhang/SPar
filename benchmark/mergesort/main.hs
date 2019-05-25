module Ms where
import Spar

instance GenRange Int where
    lBound = minBound
    rBound = maxBound

main :: IO ()    
main = benchmarkEntry "benchmark/mergesort" 3 [1] [0..3] [0..20] mergeSort