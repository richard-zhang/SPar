module Bench where
import           System.Random
import           Data.List

import           Lib
import           CodeGen
import           ParPattern
import           System.FilePath

class Range a where
    lBound :: a
    rBound :: a

instance Range Int where
    lBound = 1
    rBound = 100

randomlist :: (Range a, Random a) => Int -> StdGen -> [a]
randomlist n = take n . unfoldr (Just . randomR (lBound, rBound))

getList :: (Range a, Random a) => Int -> Int -> [a]
getList seed size = randomlist (2 ^ size) (mkStdGen seed)

benchmarkList
    :: (Serialise a, Random a, Range a)
    => FilePath
    -> Int
    -> ArrowPipe [a] b
    -> IO Double
benchmarkList path size expr = codeGenBuildRunBench (getList 100 size)
                                                    (runPipe1 zero expr)
                                                    (path </> show size)