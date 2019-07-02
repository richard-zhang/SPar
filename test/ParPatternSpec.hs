module ParPatternSpec where

import Test.Hspec
import Lib
import Example.Mergesort

testPmap :: [Double]
testPmap = codeGenTest (1, (1, (1, 1))) (paraMap (arr $ Prim "result" undefined :: ArrowPipe Int Int) ) "benchmark/pmap/test"

pmapSatisfy :: [Double] -> Bool
pmapSatisfy dat = dat !! 0 == 5 && dat !! 1 == 5 && dat !! 2 < 6

testMergeSort1 = codeGenTest [16, 15 ..1] (mergeSort 1) "benchmark/mergesort/test"

testMergeSort2 = codeGenTest [49, 48 ..1] (mergeSort 3) "benchmark/mergesort/test"

spec :: Spec
spec = do
    it "pmap" $ testPmap `shouldSatisfy` pmapSatisfy
    it "ms1"  $ testMergeSort1 `shouldSatisfy` (\x -> x !! 0 == 1 && x !! 1 == 16)
    it "ms2"  $ testMergeSort2 `shouldSatisfy` (\x -> x !! 0 == 1 && x !! 1 == 49)