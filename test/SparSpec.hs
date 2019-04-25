module SparSpec where

import Spar
import Test.Hspec

spec :: Spec
spec = do
    it "easy send receive" $ (codeGenBuildRun cgts1) `shouldReturn` True
    it "easy select branch" $ (codeGenBuildRun cgts2) `shouldReturn` True
    it "easy rec" $ (codeGenBuildRun cgts4) `shouldReturn` True
    it "easy branchCont" $ (codeGenBuildRun cgts5) `shouldReturn` True
    it "easy broadcast" $ (codeGenBuildRun cgts6) `shouldReturn` True
    it "mid rec sum type 2" $ (codeGenBuildRun cgts7) `shouldReturn` True
    it "easy rec sum type 2" $ (codeGenBuildRun cgts8) `shouldReturn` True
    it "easy pattern" $ (codeGenBuildRun' helloWorld3) `shouldReturn` True
    it "easy list data send recv" $ (codeGenBuildRun cgts9) `shouldReturn` True
    it "easy send prod data" $ (codeGenBuildRun cgts10) `shouldReturn` True
    it "mid send rec prod data" $ (codeGenBuildRun cgts11) `shouldReturn` True