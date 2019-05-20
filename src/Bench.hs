{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Bench where
import           System.Random
import           Data.List
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Diagrams
import           System.FilePath
import qualified Data.Vector                   as Vec
import qualified Statistics.Sample             as Stat
import           Data.Csv                hiding ( (.=) )
import qualified Data.Csv                      as Csv
                                                ( (.=) )
import qualified Data.ByteString.Lazy          as B
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map


import           Lib
import           CodeGen
import           ParPattern

class GenRange a where
    lBound :: a
    rBound :: a

instance GenRange Int where
    lBound = -2147483648
    rBound = 2147483647

type Benchable a = (Serialise a, Random a, GenRange a)

randomlist :: (GenRange a, Random a) => Int -> StdGen -> [a]
randomlist n = take n . unfoldr (Just . randomR (lBound, rBound))

getList :: (GenRange a, Random a) => Int -> Int -> [a]
getList seed size = randomlist (2 ^ size) (mkStdGen seed)

benchmarkList
    :: Benchable a
    => FilePath
    -> Int
    -> Int
    -> Int
    -> (Int -> ArrowPipe [a] b)
    -> IO Double
benchmarkList path seed unroll size expr = codeGenBuildRunBench
    (getList seed size)
    (runPipe1 zero (expr unroll))
    (path </> (show size ++ "_" ++ show unroll))

benchmarkSeeds
    :: Benchable a
    => FilePath
    -> [Int]
    -> Int
    -> Int
    -> (Int -> ArrowPipe [a] b)
    -> IO (Double, Double)
benchmarkSeeds path seeds unroll size expr =
    toBenchData <$> mapM (\x -> benchmarkList path x unroll size expr) seeds

benchmarkUnRolls
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> Int
    -> (Int -> ArrowPipe [a] b)
    -> IO [(Int, (Double, Double))]
benchmarkUnRolls path seeds unrolls size expr = fmap (zip unrolls)
    $ mapM (\x -> benchmarkSeeds path seeds x size expr) unrolls

benchmarkStart
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe [a] b)
    -> IO [(Int, [(Int, (Double, Double))])]
benchmarkStart path seeds unrolls sizes expr = fmap (zip sizes)
    $ mapM (\x -> benchmarkUnRolls path seeds unrolls x expr) sizes

benchmarking
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe [a] b)
    -> IO ()
benchmarking path seeds unrolls sizes expr = do
    rawData <- benchmarkStart path seeds unrolls sizes expr
    writeBenchmarkCSV path rawData
    writeBenchmarkPlot path rawData

mean :: [Double] -> Double
mean = (Stat.mean . Vec.fromList)

stdDev :: [Double] -> Double
stdDev = (Stat.stdDev . Vec.fromList)

toBenchData :: [Double] -> (Double, Double)
toBenchData xs = (mean xs, stdDev xs)

writeBenchmarkPlot :: FilePath -> [(Int, [(Int, (Double, Double))])] -> IO ()
writeBenchmarkPlot path sourceData = toFile def imagePath $ do
    layout_title .= "benchmark"
    (plotLines . toPlottableData) sourceData
    where imagePath = path </> "bench.svg"

plotLines :: [(Int, [(Int, Double)])] -> EC (Layout Int Double) ()
plotLines = mapM_ (uncurry plotLine)
  where
    plotLine :: Int -> [(Int, Double)] -> EC (Layout Int Double) ()
    plotLine title xs = plot (line (show title) [xs])

toPlottableData
    :: [(Int, [(Int, (Double, Double))])] -> [(Int, [(Int, Double)])]
toPlottableData = helper . flattenMap
  where
    helper :: (Ord a, Ord b) => [(a, b, Double)] -> [(b, [(a, Double)])]
    helper source = Map.toList $ foldl
        (\map (key1, key2, val) -> Map.insertWith (++) key2 [(key1, val)] map)
        (Map.empty)
        source

    flattenMap :: [(Int, [(Int, (Double, Double))])] -> [(Int, Int, Double)]
    flattenMap source =
        [ (key1, key2, d1) | (key1, val) <- source, (key2, (d1, _d2)) <- val ]

writeBenchmarkCSV :: FilePath -> [(Int, [(Int, (Double, Double))])] -> IO ()
writeBenchmarkCSV path sourceData = B.writeFile csvPath csvRaw
  where
    records :: [BenchData]
    records = concatMap (\(x, y) -> toRecordHelper x y) sourceData

    toRecordHelper :: Int -> [(Int, (Double, Double))] -> [BenchData]
    toRecordHelper x xs = fmap
        (\(unroll, (meanVal, stdVal)) -> BenchData (x, unroll, meanVal, stdVal))
        xs

    csvRaw  = encodeByName csvTitle records

    csvPath = path </> "raw.csv"

newtype BenchData = BenchData (Int, Int, Double, Double)

instance ToNamedRecord BenchData where
    toNamedRecord (BenchData (size, unroll, meanVal, stdVal)) = namedRecord
        [ "size" Csv..= size
        , "unroll" Csv..= unroll
        , "mean" Csv..= meanVal
        , "std" Csv..= stdVal
        ]

csvTitle :: Vec.Vector Name
csvTitle = Vec.fromList ["size", "unroll", "mean", "std"]
