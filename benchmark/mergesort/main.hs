module Ms where
import Spar

main :: IO ()    
main = benchmarkEntry "benchmark/mergesort" 1 [1] [0..3] [10..20] mergeSort