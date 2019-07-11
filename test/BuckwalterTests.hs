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
      it ("should return a lowercase t for " ++
          "ARABIC LETTER TEH") $ do
        romanization 'ت' `shouldBe` 't'
      it ("should return a lowercase v for " ++
          "ARABIC LETTER THEH") $ do
        romanization 'ث' `shouldBe` 'v'
      it ("should return a lowercase j for " ++
          "ARABIC LETTER JEEM") $ do
        romanization 'ج' `shouldBe` 'j'
      it ("should return an uppercase H for " ++
          "ARABIC LETTER HAH") $ do
        romanization 'ح' `shouldBe` 'H'
      it ("should return a lowercase x for " ++
          "ARABIC LETTER KHAH") $ do
        romanization 'خ' `shouldBe` 'x'
      it ("should return a lowercase d for " ++
          "ARABIC LETTER DAL") $ do
        romanization 'د' `shouldBe` 'd'
      it ("should return an asterisk for " ++
          "ARABIC LETTER THAL") $ do
        romanization 'ذ' `shouldBe` '*'
      it ("should return a lowercase r for " ++
          "ARABIC LETTER REH") $ do
        romanization 'ر' `shouldBe` 'r'
      it ("should return a lowercase z for " ++
          "ARABIC LETTER ZAIN") $ do
        romanization 'ز' `shouldBe` 'z'
      it ("should return a lowercase s for " ++
          "ARABIC LETTER SEEN") $ do
        romanization 'س' `shouldBe` 's'
      it ("should return a dollar sign $ for " ++
          "ARABIC LETTER SHEEN") $ do
        romanization 'ش' `shouldBe` '$'
      it ("should return a capital S for " ++
          "ARABIC LETTER SAD") $ do
        romanization 'ص' `shouldBe` 'S'
      it ("should return a capital D for " ++
          "ARABIC LETTER DAD") $ do
        romanization 'ض' `shouldBe` 'D'
      it ("should return a capital T for " ++
          "ARABIC LETTER TAH") $ do
        romanization 'ط' `shouldBe` 'T'
      it ("should return a capital Z for " ++
          "ARABIC LETTER ZAH") $ do
        romanization 'ظ' `shouldBe` 'Z'
      it ("should return a capital E for " ++
          "ARABIC LETTER AIN") $ do
        romanization 'ع' `shouldBe` 'E'
      it ("should return a lowercase g for " ++
          "ARABIC LETTER GHAIN") $ do
        romanization 'غ' `shouldBe` 'g'
      it ("should return an underscore _ for " ++
          "ARABIC TATWEEL") $ do
        romanization 'ـ' `shouldBe` '_'
      it ("should return a lowercase f for " ++
          "ARABIC LETTER FEH") $ do
        romanization 'ف' `shouldBe` 'f'
      it ("should return a lowercase q for " ++
          "ARABIC LETTER QAF") $ do
        romanization 'ق' `shouldBe` 'q'
      it ("should return a lowercase k for " ++
          "ARABIC LETTER KAF") $ do
        romanization 'ك' `shouldBe` 'k'
      it ("should return a lowercase l for " ++
          "ARABIC LETTER LAM") $ do
        romanization 'ل' `shouldBe` 'l'
      it ("should return a lowercase m for " ++
          "ARABIC LETTER MEEM") $ do
        romanization 'م' `shouldBe` 'm'
      it ("should return a lowercase n for " ++
          "ARABIC LETTER NOON") $ do
        romanization 'ن' `shouldBe` 'n'
      it ("should return a lowercase h for " ++
          "ARABIC LETTER HEH") $ do
        romanization 'ه' `shouldBe` 'h'
      it ("should return a lowercase w for " ++
          "ARABIC LETTER WAW") $ do
        romanization 'و' `shouldBe` 'w'
      it ("should return a capital Y for " ++
          "ARABIC LETTER ALEF MAKSURA") $ do
        romanization 'ى' `shouldBe` 'Y'
      it ("should return a lowercase y for " ++
          "ARABIC LETTER YEH") $ do
        romanization 'ي' `shouldBe` 'y'
      it ("should return a capital F for " ++
          "ARABIC FATHATAN") $ do
         romanization 'ً' `shouldBe` 'F'
      it ("should return a capital N for " ++
          "ARABIC DAMMATAN") $ do
         romanization 'ٌ' `shouldBe` 'N'
      it ("should return a capital K for " ++
          "ARABIC KASRATAN") $ do
         romanization 'ٍ' `shouldBe` 'K'
      it ("should return a lowercase a for " ++
          "ARABIC FATHA") $ do
        romanization 'َ' `shouldBe` 'a'
      it ("should return a lowercase u for " ++
          "ARABIC DAMMA") $ do
        romanization 'ُ' `shouldBe` 'u'
      it ("should return a lowercase i for " ++
          "ARABIC KASRA") $ do
        romanization 'ِ' `shouldBe` 'i'
      it ("should return tilde for " ++
          "ARABIC SHADDA") $ do
        romanization 'ّ' `shouldBe` '~'
      it ("should return o for " ++
          "ARABIC SUKUN")  $ do
        romanization 'ْ' `shouldBe` 'o'
      it ("should return backtick for " ++
          "ARABIC LETTER SUPERSCRIPT ALEF") $ do
        romanization 'ٰ' `shouldBe` '`'
      it ("should return left brace for " ++
          "ARABIC LETTER ALEF WASLA") $ do
        romanization 'ٱ' `shouldBe` '{'
      it ("should return capital P for " ++
          "ARABIC LETTER PEH") $ do
        romanization 'پ' `shouldBe` 'P'
      it ("should return capital J for " ++
          "ARABIC LETTER TCHEH") $ do
        romanization 'چ' `shouldBe` 'J'
      it ("should return capital V for " ++
          "ARABIC LETTER VEH") $ do
        romanization 'ڤ' `shouldBe` 'V'
      it ("should return capital G for " ++
          "ARABIC LETTER GAF") $ do
        romanization 'گ' `shouldBe` 'G'
