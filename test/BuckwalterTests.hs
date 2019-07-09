module BuckwalterTests where

import Test.Hspec

buckwalterSpecs :: Spec
buckwalterSpecs =
  do
    describe "dummySpec" $ do
      it "should just be true" $ do
        True `shouldBe` True
