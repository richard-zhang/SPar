module Main where
import Spar

main :: IO ()
-- main = codeGenDebug False testArr
-- main = codeGenDebug1 False testArr1
main = benchmarkList "codegen" 17 expr >>= print