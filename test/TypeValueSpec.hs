module TypeValueSpec where

import Data
import TypeValue
import Test.Hspec

spec :: Spec
spec = return ()

testProjection :: Spec
testProjection = do 
    it "send matched" $ do
        project (singleSend role1) role1 `shouldBe` singleSend role1
    it "send not matched" $ do
        project (singleSend role2) role1 `shouldBe` end