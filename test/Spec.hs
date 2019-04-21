-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec

import qualified LibSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cg:" LibSpec.spec