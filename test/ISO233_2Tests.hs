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
          "for ARABIC LETTER THEH") $ do
        romanization_iso232 'ث' `shouldBe` 'ṯ'
      it ("should return LATIN SMALL LETTER G WITH CARON " ++
          "for ARABIC LETTER JEEM") $ do
         romanization_iso232 'ج' `shouldBe` 'ǧ'
      it ("should return LATIN SMALL LETTER H WITH DOT BELOW " ++
          "for ARABIC LETTER HAH") $ do
         romanization_iso232 'ح' `shouldBe` 'ḥ'
      it ("should return LATIN SMALL LETTER H WITH LINE BELOW " ++
          "for ARABIC LETTER KHAH") $ do
         romanization_iso232 'خ' `shouldBe` 'ẖ'
      it ("should return LATIN SMALL LETTER D " ++
          "for ARABIC LETTER DAL") $ do
         romanization_iso232 'د' `shouldBe` 'd'
      it ("should return LATIN SMALL LETTER D WITH LINE BELOW " ++
          "for ARABIC LETTER THAL") $ do
         romanization_iso232 'ذ' `shouldBe` 'ḏ'
      it ("should return LATIN SMALL LETTER R " ++
          "for ARABIC LETTER REH") $ do
         romanization_iso232 'ر' `shouldBe` 'r'
      it ("should return LATIN SMALL LETTER Z " ++
          "for ARABIC LETTER ZAIN") $ do
          romanization_iso232 'ز' `shouldBe` 'z'
      it ("should return LATIN SMALL LETTER S " ++
          "for ARABIC LETTER SEEN") $ do
          romanization_iso232 'س' `shouldBe` 's'
      it ("should return LATIN SMALL LETTER S WITH CARON " ++
          "for ARABIC LETTER SHEEN") $ do
          romanization_iso232 'ش' `shouldBe` 'š'