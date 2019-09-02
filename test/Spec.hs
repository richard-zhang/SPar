-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec

import qualified SparSpec
import qualified ParPatternSpec
import qualified CodeGen.DataSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cg:" SparSpec.spec
  describe "arrow:" ParPatternSpec.spec
  describe "isNested:" CodeGen.DataSpec.spec