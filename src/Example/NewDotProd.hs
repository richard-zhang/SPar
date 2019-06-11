module Example.NewDotProd where
import           Lib
import           Example.DotProd

addition :: ArrowPipe (Int, Int) Int
addition = arr $ Prim "addition" undefined

split4 :: Serialise a => ArrowPipe a (a, (a, (a, a)))
split4 = arr $ Prim "split4" undefined

split2 :: Serialise a => ArrowPipe a (a, a)
split2 = arr $ Prim "split2" undefined

coalg :: (Serialise a, Serialise b) => ArrowPipe a b 
coalg = arr $ Prim "coalg" undefined

reducer :: ArrowPipe (Int, (Int, (Int, Int))) Int
reducer = arr $ Prim "reducer" undefined

newDotProd :: Int -> ArrowPipe ([Int], [Int]) Int
newDotProd 0 = arr baseFunc
newDotProd 1 = split2 >>> (arr baseFunc *** arr baseFunc) >>> addition
newDotProd 2 = pmap (arr baseFunc) split4 reducer
newDotProd _ = undefined


testNewDotProd = runPipe1 zero (newDotProd 2)