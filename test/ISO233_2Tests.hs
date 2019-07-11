-- Implementation of the transliteration system:
-- named ISO 233-2.
module ISO233_2Tests where

import Test.Hspec

import ISO233_2


iso233_2Specs :: Spec
iso233_2Specs = 
  do
    describe "romanization_iso232" $ do
      it ("should return an MODIFIER LETTER RIGHT HALF RING" ++
          "(ʾ) for ARABIC LETTER HAMZA") $ do
        romanization_iso232 'ء' `shouldBe` 'ʾ'
