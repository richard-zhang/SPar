module Example.FFT where
import Lib

testZipWith :: IO [Double]
testZipWith = codeGenTest2 (src, src2) (swapAway proxy proxy sTwo padd) "benchmark/zipwith/test"
    where
        padd = Prim "result" undefined :: Core ((Int, Int) -> Int)
        proxy = Proxy :: Proxy Int
        src = ((1, 2), (3, 4))
        src2 = ((4, 3), (2, 1))