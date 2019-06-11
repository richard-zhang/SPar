module Main where
import Spar
    
    -- instance GenRange Int where
    --     lBound = minBound
    --     rBound = maxBound
    
main :: IO ()
main = benchmarkEntry "benchmark/newdotprod" 1 [1,2,3] [0..2] [15..22] newDotProd