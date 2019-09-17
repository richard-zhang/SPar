{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Bench where
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
import           Example.Mergesort
import           Example.DotProd
import           Example.Wordcount
import           CodeGen.Data

class RandomGenData a where
    genData :: Int -> Int -> IO a

class GenBenchData a where
    mkBenchData :: Int -> BenchRawData a

instance (GenBenchData a, GenBenchData b) => GenBenchData (a, b) where
    mkBenchData len = RPair (mkBenchData len) (mkBenchData len)

instance GenBenchData [a] where
    mkBenchData len = RList len

instance RandomGenData a => RandomGenData (a, a) where
    genData x y = do
        first  <- genData x y
        second <- genData x y
        return (first, second)

instance RandomGenData [Int] where
    genData x _y = return [x]

type Benchable a = (Serialise a, GenBenchData a)

benchmarkCompile
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe a b)
    -> IO ()
benchmarkCompile path seeds unrolls sizes aexpr = mapM_ benchmarkUnRolls sizes
  where
    benchmarkUnRolls size = mapM_ (benchmarkSeeds size) unrolls
    benchmarkSeeds size unroll = mapM_ (benchmarkList size unroll) seeds
    benchmarkList size unroll seed = do
        let x = mkBenchData (2 ^ size)
        codeGenCompileWithHeader
            x
            (runPipe1 zero (aexpr unroll))
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

benchmarkRun2 :: FilePath -> Int -> [Int] -> [Int] -> [Int] -> IO ()
benchmarkRun2 path time seeds unrolls sizes =
    (toRawData <$> rawData)
        >>= (B.writeFile (path </> rawDataName2) . Csv.encode)
  where
    toRawData xs =
        [ RawData size' 0 unroll' (fst val)
        | (size'  , rest) <- xs
        , (unroll', val ) <- rest
        ]

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

rawDataName2 :: String
rawDataName2 = "rawData.csv"

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

benchmarkCollectData2 :: FilePath -> IO ()
benchmarkCollectData2 path = do
    rawData <- rawDataDecode path
    -- removeFile (path </> rawDataName2)
    -- writeBenchmarkCSV path rawData
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

data RawData = RawData { size :: Int, seed :: Int, unroll :: Int, time :: Double }

instance Csv.FromRecord RawData where
    parseRecord v =
        RawData <$> v Csv..! 0 <*> v Csv..! 1 <*> v Csv..! 2 <*> v Csv..! 3

instance Csv.ToRecord RawData where
    toRecord RawData {..} = Csv.record
        [ Csv.toField size
        , Csv.toField seed
        , Csv.toField unroll
        , Csv.toField time
        ]

rawDataDecode :: FilePath -> IO [(Int, [(Int, (Double, Double))])]
rawDataDecode path = do
    rawData <- B.readFile filePath
    let decoder =
            (Csv.decode Csv.NoHeader rawData) :: Either
                    String
                    (Vec.Vector RawData)
    return $ rawDatadecode' decoder
  where
    filePath = path </> rawDataName2
    rawDatadecode'
        :: Either String (Vec.Vector RawData)
        -> [(Int, [(Int, (Double, Double))])]
    rawDatadecode' x = case x of
        Left  msg        -> error $ "failed " ++ msg
        Right sourceData -> (toList . toMap'' . toMap' . toMap) sourceData

    toMap = Vec.foldl
        (\map record ->
            Map.insertWith (++) (size record, unroll record) [time record] map
        )
        Map.empty

    toMap'' = Map.foldrWithKey
        (\(mySize, myUnroll) val acc ->
            Map.insertWith (Map.union) mySize (Map.singleton myUnroll val) acc
        )
        Map.empty

    toMap' = Map.map toBenchData

    toList = (Map.toList . Map.map Map.toList)

csvTitle :: Vec.Vector Csv.Name
csvTitle = Vec.fromList ["size", "unroll", "mean", "std"]

runParser :: Parser (Bool, Bool)
runParser =
    (,)
        <$> switch (long "collect" <> short 'c' <> help "whether to collect")
        <*> switch (long "run" <> short 'r' <> help "whether to run or codegen")

genBenchmarkFile
    :: Benchable a
    => FilePath
    -> [Int]
    -> [Int]
    -> (Int -> ArrowPipe a b)
    -> IO ()
genBenchmarkFile path unrolls sizes expr =
    benchmarkEntry path 1 [1] unrolls sizes expr

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
        (True , _    ) -> benchmarkCollectData2 path
        (False, True ) -> benchmarkRun2 path time seeds unrolls sizes
        (False, False) -> benchmarkCompile path seeds unrolls sizes expr
    where opts = info (runParser <**> helper) mempty

benchmarkNewEntry :: FilePath -> Int -> [Int] -> [Int] -> [Int] -> IO ()
benchmarkNewEntry mainPath time seeds unrolls sizes =
    execParser opts >>= \x -> case x of
        (True, _) ->
            mapM_ (\path -> benchmarkCollectData (mainPath </> path)) names
        (False, True) -> mapM_
            (\path -> benchmarkRun (mainPath </> path) time seeds unrolls sizes)
            names
        (False, False) ->
            mapM_ (\name -> myHelper name (mainPath </> name)) names
  where
    names = ["intcount", "mergesort", "dotprod"]
    opts  = info (runParser <**> helper) mempty

    myHelper "intcount" path =
        benchmarkCompile path seeds unrolls sizes wordCount
    myHelper "mergesort" path =
        benchmarkCompile path seeds unrolls sizes mergeSort
    myHelper "dotprod" path = benchmarkCompile path seeds unrolls sizes dotProd
    myHelper _         _    = error "benchmark example not implemented"
