{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench where
import           System.Random
import           Data.List
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Diagrams
import           System.FilePath
import           System.Directory
import qualified Data.Vector                   as Vec
import qualified Statistics.Sample             as Stat
import qualified Data.Csv                      as Csv
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map                      as Map
import           Data.Type.Natural
import           Data.List                      ( intercalate )
import qualified Data.Store                    as Store
                                         hiding ( size )
import qualified Data.ByteString               as Bs
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

import           CodeGen.Type
import           ParPattern
import           CodeGen

class GenRange a where
    lBound :: a
    rBound :: a

class RandomGenData a where
    genData :: Int -> Int -> IO a

instance RandomGenData a => RandomGenData (a, a) where
    genData x y = do
        first  <- genData x y
        second <- genData x y
        return (first, second)

instance (GenRange a, Random a) => RandomGenData [a] where
    genData x y = randomList2 x (mkStdGen y)

type Benchable a = (Serialise a, RandomGenData a)

randomList2 :: (GenRange a, Random a) => Int -> StdGen -> IO [a]
randomList2 n _ =
    newStdGen >>= return . take n . unfoldr (Just . randomR (lBound, rBound))

randomlist :: (GenRange a, Random a) => Int -> StdGen -> [a]
randomlist n = take n . unfoldr (Just . randomR (lBound, rBound))

getList :: (GenRange a, Random a) => Int -> Int -> [a]
getList seed size = randomlist (2 ^ size) (mkStdGen seed)

benchmarkStart
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe a b)
    -> IO [(Int, [(Int, (Double, Double))])]
benchmarkStart path seeds unrolls sizes expr = fmap (zip sizes)
    $ mapM benchmarkUnRolls sizes
  where
    benchmarkUnRolls size =
        fmap (zip unrolls) $ mapM (benchmarkSeeds size) unrolls
    benchmarkSeeds size unroll =
        toBenchData <$> mapM (benchmarkList size unroll) seeds
    benchmarkList size unroll seed = do
        x <- genData (2 ^ size) seed
        codeGenBuildRunBench
            x
            (runPipe1 zero (expr unroll))
            (path </> (intercalate "_" [show size, show unroll, show seed]))

benchmarkCompile
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe a b)
    -> IO ()
benchmarkCompile path seeds unrolls sizes expr = mapM_ benchmarkUnRolls sizes
  where
    benchmarkUnRolls size = mapM_ (benchmarkSeeds size) unrolls
    benchmarkSeeds size unroll = mapM_ (benchmarkList size unroll) seeds
    benchmarkList size unroll seed = do
        x <- genData (2 ^ size) seed
        codeGenBenchCompile
            x
            (runPipe1 zero (expr unroll))
            (path </> (intercalate "_" [show size, show unroll, show seed]))

benchmarkRun :: FilePath -> Int -> [Int] -> [Int] -> [Int] -> IO ()
benchmarkRun path time seeds unrolls sizes =
    rawData >>= (Bs.writeFile (path </> rawDataName) . Store.encode)
  where
    rawData = fmap (zip sizes) $ mapM benchmarkUnRolls sizes
    benchmarkUnRolls size =
        fmap (zip unrolls) $ mapM (benchmarkSeeds size) unrolls
    benchmarkSeeds size unroll =
        toBenchData <$> (concat <$> mapM (benchmarkList size unroll) seeds)
    benchmarkList size unroll seed = mapM
        (const $ codeGenBenchRun
            (path </> (intercalate "_" [show size, show unroll, show seed]))
        )
        [1 .. time]

rawDataName :: String
rawDataName = "rawData"

benchmarkCollectData :: FilePath -> IO ()
benchmarkCollectData path = do
    rawDataRead <- Bs.readFile (path </> rawDataName)
    let rawData = case Store.decode rawDataRead of
            Left  x -> error $ show x
            Right y -> y
    removeFile (path </> rawDataName)
    writeBenchmarkCSV path rawData
    writeBenchmarkPlot path rawData
    writeBenchmarkSpeedUpPlot path rawData

benchmarking
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe a b)
    -> IO ()
benchmarking path seeds unrolls sizes expr = do
    benchmarkCompile path seeds unrolls sizes expr
    benchmarkRun path 1 seeds unrolls sizes
    benchmarkCollectData path

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

-- the first int is size
writeBenchmarkSpeedUpPlot
    :: FilePath -> [(Int, [(Int, (Double, Double))])] -> IO ()
writeBenchmarkSpeedUpPlot path sourceData = toFile def imagePath $ do
    layout_title .= "Speedup"
    (plotLines . toPlottableData . toSpeedUpData) sourceData
    where imagePath = path </> "benchSpeedup.svg"

-- the first int is k
plotLines :: [(Int, [(Int, Double)])] -> EC (Layout Int Double) ()
plotLines = mapM_ (uncurry plotLine)
  where
    plotLine :: Int -> [(Int, Double)] -> EC (Layout Int Double) ()
    plotLine title xs = plot (line ("k" ++ show title) [xs])

toSpeedUpData
    :: [(Int, [(Int, (Double, Double))])] -> [(Int, [(Int, (Double, Double))])]
toSpeedUpData = fmap (\(x, y) -> (x, helper helper2 y))
  where
    helper2
        :: (Int, (Double, Double))
        -> (Int, (Double, Double))
        -> (Int, (Double, Double))
    helper2 (size, parTime) (_, serTime) =
        (size, (fst serTime / fst parTime, snd parTime))

    helper f xs = fmap (\x -> f x (head xs)) (tail xs)

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

    csvRaw  = Csv.encodeByName csvTitle records

    csvPath = path </> "bench.csv"

newtype BenchData = BenchData (Int, Int, Double, Double)

instance Csv.ToNamedRecord BenchData where
    toNamedRecord (BenchData (size, unroll, meanVal, stdVal)) = Csv.namedRecord
        [ "size" Csv..= size
        , "unroll" Csv..= unroll
        , "mean" Csv..= meanVal
        , "std" Csv..= stdVal
        ]

csvTitle :: Vec.Vector Csv.Name
csvTitle = Vec.fromList ["size", "unroll", "mean", "std"]

runParser :: Parser (Bool, Bool)
runParser =
    (,)
        <$> switch (long "collect" <> short 'c' <> help "whether to collect")
        <*> switch (long "run" <> short 'r' <> help "whether to run or codegen")


benchmarkEntry
    :: Benchable a
    => FilePath
    -> Int
    -> [Int]
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe a b)
    -> IO ()
benchmarkEntry path time seeds unrolls sizes expr = execParser opts >>= \x ->
    case x of
        (True , _    ) -> benchmarkCollectData path
        (False, True ) -> benchmarkRun path time seeds unrolls sizes
        (False, False) -> benchmarkCompile path seeds unrolls sizes expr
    where opts = info (runParser <**> helper) mempty
