module Main where
import Spar



main :: IO ()
-- main = benchmarking "benchmark/intcount" [100, 99, 101, 98, 103] [0..2] [15..20] wordCount
-- main = benchmarkEntry "benchmark/intcount" 10 [1] [0..3] [0..20] wordCount
main = benchmarkEntry "benchmark/intcount" 1 [0..30] [0 .. 2] [20 .. 24] wordCount