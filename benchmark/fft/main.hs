module Main where
import Spar

main :: IO ()
main = genBenchmarkFile "benchmark/fft" [0 .. 6] [15 .. 24] fft