-- Implementation of the transliteration system:
-- named ISO 233-2.
module ISO233_2Tests where

import Test.Hspec

import ISO233_2
-- For names of Latin characters: https://unicode.org/charts/PDF/U0000.pdf
-- For names of Arabic characters: http://unicode.org/charts/PDF/U0600.pdf

iso233_2Specs :: Spec
iso233_2Specs = 
  do
    describe "romanization_iso232" $ do
      it ("should return MODIFIER LETTER RIGHT HALF RING " ++
          "(ʾ) for ARABIC LETTER HAMZA") $ do
        romanization_iso232 'ء' `shouldBe` 'ʾ'
      it ("should return LATIN SMALL LETTER B " ++
          "(b) for ARABIC LETTER BEH") $ do
        romanization_iso232 'ب' `shouldBe` 'b'
      it ("should return LATIN SMALL LETTER T " ++
          "(t) for ARABIC LETTER TEH") $ do
        romanization_iso232 'ت' `shouldBe` 't'
      it ("should return LATIN SMALL LETTER T WITH LINE BELOW " ++
          " for ARABIC LETTER THEH") $ do
        romanization_iso232 'ث' `shouldBe` 'ṯ'
      it ("should return LATIN SMALL LETTER G WITH CARON " ++
          " for ARABIC LETTER JEEM") $ do
         romanization_iso232 'ج' `shouldBe` 'ǧ'