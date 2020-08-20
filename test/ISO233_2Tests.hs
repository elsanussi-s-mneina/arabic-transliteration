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
      it ("should return LATIN SMALL LETTER S WITH DOT BELOW " ++
          "for ARABIC LETTER SAD") $ do
          romanization_iso232 'ص' `shouldBe` 'ṣ'
      it ("should return LATIN SMALL LETTER D WITH DOT BELOW " ++
          "for ARABIC LETTER DAD") $ do
          romanization_iso232 'ض' `shouldBe` 'ḍ'
      it ("should return LATIN SMALL LETTER T WITH DOT BELOW " ++
          "for ARABIC LETTER TAH") $ do
          romanization_iso232 'ط' `shouldBe` 'ṭ'
      it ("should return LATIN SMALL LETTER Z WITH DOT BELOW " ++
          "for ARABIC LETTER ZAH") $ do
          romanization_iso232 'ظ' `shouldBe` 'ẓ'
      it ("should return MODIFIER LETTER LEFT HALF RING " ++
          "for ARABIC LETTER AIN") $ do
          romanization_iso232 'ع' `shouldBe` 'ʿ'
      it ("should return LATIN SMALL LETTER G WITH DOT ABOVE " ++
          "for ARABIC LETTER GHAIN") $ do
          romanization_iso232 'غ' `shouldBe` 'ġ'
      it ("should return LATIN SMALL LETTER F " ++
          "for ARABIC LETTER FEH") $ do
          romanization_iso232 'ف' `shouldBe` 'f'
      it ("should return LATIN SMALL LETTER Q " ++
          "for ARABIC LETTER QAF") $ do
          romanization_iso232 'ق' `shouldBe` 'q'
      it ("should return LATIN SMALL LETTER K " ++
          "for ARABIC LETTER KAF") $ do
          romanization_iso232 'ك' `shouldBe` 'k'
      it ("should return LATIN SMALL LETTER L " ++
          "for ARABIC LETTER LAM") $ do
          romanization_iso232 'ل' `shouldBe` 'l'
      it ("should return LATIN SMALL LETTER M " ++
          "for ARABIC LETTER MEEM") $ do
          romanization_iso232 'م' `shouldBe` 'm'
      it ("should return LATIN SMALL LETTER N " ++
          "for ARABIC LETTER NOON") $ do
          romanization_iso232 'ن' `shouldBe` 'n'
      it ("should return LATIN SMALL LETTER H " ++
          "for ARABIC LETTER HEH") $ do
          romanization_iso232 'ه' `shouldBe` 'h'
      it ("should return LATIN SMALL LETTER T WITH DIAERESIS " ++
          "for ARABIC LETTER TEH MARBUTA") $ do
          romanization_iso232 'ة' `shouldBe` 'ẗ'
      it ("should return LATIN SMALL LETTER W " ++
          "for ARABIC LETTER WAW") $ do
          romanization_iso232 'و' `shouldBe` 'w'
      it ("should return LATIN SMALL LETTER Y " ++
          "for ARABIC LETTER YEH") $ do
          romanization_iso232 'ي' `shouldBe` 'y'

      {-
      Unit test template:
      it ("should return " ++
          "for ") $ do
          romanization_iso232 '' `shouldBe` ''
      -}