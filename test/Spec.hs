-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec

import qualified SparSpec
import qualified ParPatternSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cg:" SparSpec.spec
  describe "arrow:" ParPatternSpec.spec