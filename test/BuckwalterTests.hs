module BuckwalterTests where

import Test.Hspec

import Buckwalter

buckwalterSpecs :: Spec
buckwalterSpecs =
  do
    describe "romanization" $ do
      it "should return an apostrophe for ARABIC LETTER HAMZA" $ do
        romanization 'ء' `shouldBe` '\''
