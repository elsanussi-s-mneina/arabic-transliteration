module BuckwalterTests where

import Test.Hspec

import Buckwalter

buckwalterSpecs :: Spec
buckwalterSpecs =
  do
    describe "romanization" $ do
      it "should return an apostrophe for ARABIC LETTER HAMZA" $ do
        romanization 'ء' `shouldBe` '\''
      it "should return a pipe for ARABIC LETTER ALEF WITH MADDA ABOVE" $ do
        romanization 'آ' `shouldBe` '|'
      it ("should return a greater than sign for ARABIC LETTER ALEF " ++
          "WITH HAMZA ABOVE") $ do
        romanization 'أ' `shouldBe` '>'
      it ("should return an ampersand for ARABIC LETTER WAW " ++
          "WITH HAMZA ABOVE") $ do
        romanization 'ؤ' `shouldBe` '&'
      it ("should return a less than sign for " ++
          "ARABIC LETTER ALEF WITH HAMZA BELOW") $ do
        romanization 'إ' `shouldBe` '<'
      it ("should return a right brace for " ++
          "ARABIC LETTER YEH WITH HAMZA ABOVE") $ do
        romanization 'ئ' `shouldBe` '}'
      it ("should return a capital A for " ++
          "ARABIC LETTER ALEF") $ do
        romanization 'ا' `shouldBe` 'A'
      it ("should return a lowercase b for " ++
          "ARABIC LETTER BEH") $ do
        romanization 'ب' `shouldBe` 'b'
      it ("should return a lowercase p for " ++
          "ARABIC LETTER TEH MARBUTA") $ do
        romanization 'ة' `shouldBe` 'p'
