module TypeValueSpec where

import Test.Hspec

spec :: Spec
spec = describe "read" $ do 
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)