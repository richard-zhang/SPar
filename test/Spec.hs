{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
import Test.TypeSpec
import GHC.TypeLits
import GHC.Exts
import Control.Monad.Free
import Type 

specHelloWorld :: Explain "Expect something..." (DualityC '[ '( 'Free ('S 1 Integer ('Pure ())), 0), '( 'Free ('R 0 Integer ('Pure ())), 1) ] `Isn't` Int)
specHelloWorld = Valid

main :: IO ()
main = print specHelloWorld
