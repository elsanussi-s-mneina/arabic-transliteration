-- Implementation of the transliteration system:
-- named ISO 233-2.
module ISO232Tests where

import Test.Hspec

import ISO232


iso232Specs :: Spec
iso232Specs = 
  do
    describe "romanization_iso232" $ do
      it ("should return an MODIFIER LETTER RIGHT HALF RING" ++
          "(ʾ) for ARABIC LETTER HAMZA") $ do
        romanization_iso232 'ء' `shouldBe` 'ʾ'
