module Main where
import Spar
import Control.Monad

main :: IO ()
-- main = codeGenDebug False testArr
-- main = codeGenDebug1 False testArr1
main = forM_ [0..3] (\r -> benchmarkList "ct" r 3 mergeSort >>= print)
-- main = benchmarkList "ct" 1 4 mergeSort >>= print