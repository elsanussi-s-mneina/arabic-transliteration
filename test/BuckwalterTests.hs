module BuckwalterTests where

import Test.Hspec

import Buckwalter

buckwalterSpecs :: Spec
buckwalterSpecs =
  do
    describe "romanization" $ do
      it "should return APOSTROPHE (U+0027) for ARABIC LETTER HAMZA (U+0621)" $ do
        romanization 'ء' `shouldBe` '\''
        romanization 'ء' `shouldBe` '\x0027'
        romanization '\x0621' `shouldBe` '\''
        romanization '\x0621' `shouldBe` '\x0027'
      it "should return a VERTICAL LINE (U+007C) for ARABIC LETTER ALEF WITH MADDA ABOVE (U+0622)" $ do
        romanization 'آ' `shouldBe` '|'
        romanization '\x0622' `shouldBe` '|'
        romanization 'آ' `shouldBe` '\x007C'
        romanization '\x0622' `shouldBe` '\x007C'

      it ("should return a greater than sign for ARABIC LETTER ALEF " ++
          "WITH HAMZA ABOVE") $ do
        romanization 'أ' `shouldBe` '>'
        romanization '\x0623' `shouldBe` '>'
      it ("should return an ampersand for ARABIC LETTER WAW " ++
          "WITH HAMZA ABOVE") $ do
        romanization 'ؤ' `shouldBe` '&'
        romanization '\x0624' `shouldBe` '&'
      it ("should return a less than sign for " ++
          "ARABIC LETTER ALEF WITH HAMZA BELOW") $ do
        romanization 'إ' `shouldBe` '<'
        romanization '\x0625' `shouldBe` '<'
      it ("should return a right brace for " ++
          "ARABIC LETTER YEH WITH HAMZA ABOVE") $ do
        romanization 'ئ' `shouldBe` '}'
        romanization '\x0626' `shouldBe` '}'
      it ("should return a capital A for " ++
          "ARABIC LETTER ALEF") $ do
        romanization 'ا' `shouldBe` 'A'
        romanization '\x0627' `shouldBe` 'A'
      it ("should return a lowercase b for " ++
          "ARABIC LETTER BEH") $ do
        romanization 'ب' `shouldBe` 'b'
        romanization '\x0628' `shouldBe` 'b'
      it ("should return a lowercase p for " ++
          "ARABIC LETTER TEH MARBUTA") $ do
        romanization 'ة' `shouldBe` 'p'
        romanization '\x0629' `shouldBe` 'p'
      it ("should return a lowercase t for " ++
          "ARABIC LETTER TEH") $ do
        romanization 'ت' `shouldBe` 't'
        romanization '\x062A' `shouldBe` 't'
      it ("should return a lowercase v for " ++
          "ARABIC LETTER THEH") $ do
        romanization 'ث' `shouldBe` 'v'
        romanization '\x062B' `shouldBe` 'v'
      it ("should return a lowercase j for " ++
          "ARABIC LETTER JEEM") $ do
        romanization 'ج' `shouldBe` 'j'
        romanization '\x062C' `shouldBe` 'j'
      it ("should return an uppercase H for " ++
          "ARABIC LETTER HAH") $ do
        romanization 'ح' `shouldBe` 'H'
        romanization '\x062D' `shouldBe` 'H'
      it ("should return a lowercase x for " ++
          "ARABIC LETTER KHAH") $ do
        romanization 'خ' `shouldBe` 'x'
        romanization '\x062E' `shouldBe` 'x'
      it ("should return a lowercase d for " ++
          "ARABIC LETTER DAL") $ do
        romanization 'د' `shouldBe` 'd'
        romanization '\x062F' `shouldBe` 'd'
      it ("should return an asterisk for " ++
          "ARABIC LETTER THAL") $ do
        romanization 'ذ' `shouldBe` '*'
        romanization '\x0630' `shouldBe` '*'
      it ("should return a lowercase r for " ++
          "ARABIC LETTER REH") $ do
        romanization 'ر' `shouldBe` 'r'
        romanization '\x0631' `shouldBe` 'r'
      it ("should return a lowercase z for " ++
          "ARABIC LETTER ZAIN") $ do
        romanization 'ز' `shouldBe` 'z'
        romanization '\x0632' `shouldBe` 'z'
      it ("should return a lowercase s for " ++
          "ARABIC LETTER SEEN") $ do
        romanization 'س' `shouldBe` 's'
        romanization '\x0633' `shouldBe` 's'
      it ("should return a dollar sign $ for " ++
          "ARABIC LETTER SHEEN") $ do
        romanization 'ش' `shouldBe` '$'
        romanization '\x0634' `shouldBe` '$'
      it ("should return a capital S for " ++
          "ARABIC LETTER SAD") $ do
        romanization 'ص' `shouldBe` 'S'
        romanization '\x0635' `shouldBe` 'S'
      it ("should return a capital D for " ++
          "ARABIC LETTER DAD") $ do
        romanization 'ض' `shouldBe` 'D'
        romanization '\x0636' `shouldBe` 'D'
      it ("should return a capital T for " ++
          "ARABIC LETTER TAH") $ do
        romanization 'ط' `shouldBe` 'T'
        romanization '\x0637' `shouldBe` 'T'
      it ("should return a capital Z for " ++
          "ARABIC LETTER ZAH") $ do
        romanization 'ظ' `shouldBe` 'Z'
        romanization '\x0638' `shouldBe` 'Z'
      it ("should return a capital E for " ++
          "ARABIC LETTER AIN") $ do
        romanization 'ع' `shouldBe` 'E'
        romanization '\x0639' `shouldBe` 'E'
      it ("should return a lowercase g for " ++
          "ARABIC LETTER GHAIN") $ do
        romanization 'غ' `shouldBe` 'g'
        romanization '\x063A' `shouldBe` 'g'
      it ("should return an underscore _ for " ++
          "ARABIC TATWEEL") $ do
        romanization 'ـ' `shouldBe` '_'
        romanization '\x0640' `shouldBe` '_'
      it ("should return a lowercase f for " ++
          "ARABIC LETTER FEH") $ do
        romanization 'ف' `shouldBe` 'f'
        romanization '\x0641' `shouldBe` 'f'
      it ("should return a lowercase q for " ++
          "ARABIC LETTER QAF") $ do
        romanization 'ق' `shouldBe` 'q'
        romanization '\x0642' `shouldBe` 'q'
      it ("should return a lowercase k for " ++
          "ARABIC LETTER KAF") $ do
        romanization 'ك' `shouldBe` 'k'
        romanization '\x0643' `shouldBe` 'k'
      it ("should return a lowercase l for " ++
          "ARABIC LETTER LAM") $ do
        romanization 'ل' `shouldBe` 'l'
        romanization '\x0644' `shouldBe` 'l'
      it ("should return a lowercase m for " ++
          "ARABIC LETTER MEEM") $ do
        romanization 'م' `shouldBe` 'm'
        romanization '\x0645' `shouldBe` 'm'
      it ("should return a lowercase n for " ++
          "ARABIC LETTER NOON") $ do
        romanization 'ن' `shouldBe` 'n'
        romanization '\x0646' `shouldBe` 'n'
      it ("should return a lowercase h for " ++
          "ARABIC LETTER HEH") $ do
        romanization 'ه' `shouldBe` 'h'
        romanization '\x0647' `shouldBe` 'h'
      it ("should return a lowercase w for " ++
          "ARABIC LETTER WAW") $ do
        romanization 'و' `shouldBe` 'w'
        romanization '\x0648' `shouldBe` 'w'
      it ("should return a capital Y for " ++
          "ARABIC LETTER ALEF MAKSURA") $ do
        romanization 'ى' `shouldBe` 'Y'
        romanization '\x0649' `shouldBe` 'Y'
      it ("should return a lowercase y for " ++
          "ARABIC LETTER YEH") $ do
        romanization 'ي' `shouldBe` 'y'
        romanization '\x064A' `shouldBe` 'y'
      it ("should return a capital F for " ++
          "ARABIC FATHATAN") $ do
         romanization 'ً' `shouldBe` 'F'
         romanization '\x064B' `shouldBe` 'F'
      it ("should return a capital N for " ++
          "ARABIC DAMMATAN") $ do
         romanization 'ٌ' `shouldBe` 'N'
         romanization '\x064C' `shouldBe` 'N'
      it ("should return a capital K for " ++
          "ARABIC KASRATAN") $ do
         romanization 'ٍ' `shouldBe` 'K'
         romanization '\x064D' `shouldBe` 'K'
      it ("should return a lowercase a for " ++
          "ARABIC FATHA") $ do
        romanization 'َ' `shouldBe` 'a'
        romanization '\x064E' `shouldBe` 'a'
      it ("should return a lowercase u for " ++
          "ARABIC DAMMA") $ do
        romanization 'ُ' `shouldBe` 'u'
        romanization '\x064F' `shouldBe` 'u'
      it ("should return a lowercase i for " ++
          "ARABIC KASRA") $ do
        romanization 'ِ' `shouldBe` 'i'
        romanization '\x0650' `shouldBe` 'i'
      it ("should return tilde for " ++
          "ARABIC SHADDA") $ do
        romanization 'ّ' `shouldBe` '~'
        romanization '\x0651' `shouldBe` '~'
      it ("should return o for " ++
          "ARABIC SUKUN")  $ do
        romanization 'ْ' `shouldBe` 'o'
        romanization '\x0652' `shouldBe` 'o'
      it ("should return backtick for " ++
          "ARABIC LETTER SUPERSCRIPT ALEF") $ do
        romanization 'ٰ' `shouldBe` '`'
        romanization '\x0670' `shouldBe` '`'
      it ("should return left brace for " ++
          "ARABIC LETTER ALEF WASLA") $ do
        romanization 'ٱ' `shouldBe` '{'
        romanization '\x0671' `shouldBe` '{'
      it ("should return capital P for " ++
          "ARABIC LETTER PEH") $ do
        romanization 'پ' `shouldBe` 'P'
        romanization '\x067E' `shouldBe` 'P'
      it ("should return capital J for " ++
          "ARABIC LETTER TCHEH") $ do
        romanization 'چ' `shouldBe` 'J'
        romanization '\x0686' `shouldBe` 'J'
      it ("should return capital V for " ++
          "ARABIC LETTER VEH") $ do
        romanization 'ڤ' `shouldBe` 'V'
        romanization '\x06A4' `shouldBe` 'V'
      it ("should return capital G for " ++
          "ARABIC LETTER GAF") $ do
        romanization 'گ' `shouldBe` 'G'
        romanization '\x06AF' `shouldBe` 'G'
      it ("should return space for space") $ do
        romanization ' ' `shouldBe` ' '
        romanization '\x0020' `shouldBe` '\x0020'
      it ("should return question mark for Arabic question mark") $ do
        romanization '؟' `shouldBe` '?'
        romanization '\x061F' `shouldBe` '?'
      it ("should return comma for Arabic comma") $ do
        romanization '،' `shouldBe` ','
        romanization '\x060C' `shouldBe` ','
      it ("should return semicolon for Arabic semicolon") $ do
        romanization '؛' `shouldBe` ';'
        romanization '\x061B' `shouldBe` ';'

    describe "deromanization" $ do
      it "should when given an apostrophe return ARABIC LETTER HAMZA" $ do
        deromanization '\'' `shouldBe` 'ء'
      it "should when given a pipe return ARABIC LETTER ALEF WITH MADDA ABOVE" $ do
        deromanization '|' `shouldBe` 'آ'
      it ("should when given a greater than sign return ARABIC LETTER ALEF " ++
          "WITH HAMZA ABOVE") $ do
        deromanization '>' `shouldBe` 'أ'
      it ("should when given an ampersand return ARABIC LETTER WAW " ++
          "WITH HAMZA ABOVE") $ do
        deromanization '&' `shouldBe` 'ؤ'
      it ("should when given a less than sign return " ++
          "ARABIC LETTER ALEF WITH HAMZA BELOW") $ do
        deromanization '<' `shouldBe` 'إ'
      it ("should when given a right brace return " ++
          "ARABIC LETTER YEH WITH HAMZA ABOVE") $ do
        deromanization '}' `shouldBe` 'ئ'
      it ("should when given a capital A return " ++
          "ARABIC LETTER ALEF") $ do
        deromanization 'A' `shouldBe` 'ا'
      it ("should when given a lowercase b return " ++
          "ARABIC LETTER BEH") $ do
        deromanization 'b' `shouldBe` 'ب'
      it ("should when given a lowercase p return " ++
          "ARABIC LETTER TEH MARBUTA") $ do
        deromanization 'p' `shouldBe` 'ة'
      it ("should when given a lowercase t return " ++
          "ARABIC LETTER TEH") $ do
        deromanization 't' `shouldBe` 'ت'
      it ("should when given a lowercase v return " ++
          "ARABIC LETTER THEH") $ do
        deromanization 'v' `shouldBe` 'ث'
      it ("should when given a lowercase j return " ++
          "ARABIC LETTER JEEM") $ do
        deromanization 'j' `shouldBe` 'ج'
      it ("should when given an uppercase H return " ++
          "ARABIC LETTER HAH") $ do
        deromanization 'H' `shouldBe` 'ح'
      it ("should when given a lowercase x return " ++
          "ARABIC LETTER KHAH") $ do
        deromanization 'x' `shouldBe` 'خ'
      it ("should when given a lowercase d return " ++
          "ARABIC LETTER DAL") $ do
        deromanization 'd' `shouldBe` 'د'
      it ("should when given an asterisk return " ++
          "ARABIC LETTER THAL") $ do
        deromanization '*' `shouldBe` 'ذ'
      it ("should when given a lowercase r return " ++
          "ARABIC LETTER REH") $ do
        deromanization 'r' `shouldBe` 'ر'
      it ("should when given a lowercase z return " ++
          "ARABIC LETTER ZAIN") $ do
        deromanization 'z' `shouldBe` 'ز'
      it ("should when given a lowercase s return " ++
          "ARABIC LETTER SEEN") $ do
        deromanization 's' `shouldBe` 'س'
      it ("should when given a dollar sign $ return " ++
          "ARABIC LETTER SHEEN") $ do
        deromanization '$' `shouldBe` 'ش'
      it ("should when given a capital S return " ++
          "ARABIC LETTER SAD") $ do
        deromanization 'S' `shouldBe` 'ص'
      it ("should when given a capital D return " ++
          "ARABIC LETTER DAD") $ do
        deromanization 'D' `shouldBe` 'ض'
      it ("should when given a capital T return " ++
          "ARABIC LETTER TAH") $ do
        deromanization 'T' `shouldBe` 'ط'
      it ("should when given a capital Z return " ++
          "ARABIC LETTER ZAH") $ do
        deromanization 'Z' `shouldBe` 'ظ'
      it ("should when given a capital E return " ++
          "ARABIC LETTER AIN") $ do
        deromanization 'E' `shouldBe` 'ع'
      it ("should when given a lowercase g return " ++
          "ARABIC LETTER GHAIN") $ do
        deromanization 'g' `shouldBe` 'غ'
      it ("should when given an underscore _ return " ++
          "ARABIC TATWEEL") $ do
        deromanization '_' `shouldBe` 'ـ'
      it ("should when given a lowercase f return " ++
          "ARABIC LETTER FEH") $ do
        deromanization 'f' `shouldBe` 'ف'
      it ("should when given a lowercase q return " ++
          "ARABIC LETTER QAF") $ do
        deromanization 'q' `shouldBe` 'ق'
      it ("should when given a lowercase k return " ++
          "ARABIC LETTER KAF") $ do
        deromanization 'k' `shouldBe` 'ك'
      it ("should when given a lowercase l return " ++
          "ARABIC LETTER LAM") $ do
        deromanization 'l' `shouldBe` 'ل'
      it ("should when given a lowercase m return " ++
          "ARABIC LETTER MEEM") $ do
        deromanization 'm' `shouldBe` 'م'
      it ("should when given a lowercase n return " ++
          "ARABIC LETTER NOON") $ do
        deromanization 'n' `shouldBe` 'ن'
      it ("should when given a lowercase h return " ++
          "ARABIC LETTER HEH") $ do
        deromanization 'h' `shouldBe` 'ه'
      it ("should when given a lowercase w return " ++
          "ARABIC LETTER WAW") $ do
        deromanization 'w' `shouldBe` 'و'
      it ("should when given a capital Y return " ++
          "ARABIC LETTER ALEF MAKSURA") $ do
        deromanization 'Y' `shouldBe` 'ى'
      it ("should when given a lowercase y return " ++
          "ARABIC LETTER YEH") $ do
        deromanization 'y' `shouldBe` 'ي'
      it ("should when given a capital F return " ++
          "ARABIC FATHATAN") $ do
         deromanization 'F' `shouldBe`  'ً'
      it ("should when given a capital N return " ++
          "ARABIC DAMMATAN") $ do
         deromanization 'N' `shouldBe`  'ٌ'
      it ("should when given a capital K return " ++
          "ARABIC KASRATAN") $ do
         deromanization 'K' `shouldBe`  'ٍ'
      it ("should when given a lowercase a return " ++
          "ARABIC FATHA") $ do
        deromanization 'a' `shouldBe`  'َ'
      it ("should when given a lowercase u return " ++
          "ARABIC DAMMA") $ do
        deromanization 'u' `shouldBe`  'ُ'
      it ("should when given a lowercase i return " ++
          "ARABIC KASRA") $ do
        deromanization 'i' `shouldBe`  'ِ'
      it ("should when given tilde return " ++
          "ARABIC SHADDA") $ do
        deromanization '~' `shouldBe`  'ّ'
      it ("should when given o return " ++
          "ARABIC SUKUN")  $ do
        deromanization 'o' `shouldBe`  'ْ'
      it ("should when given backtick return " ++
          "ARABIC LETTER SUPERSCRIPT ALEF") $ do
        deromanization '`' `shouldBe` 'ٰ'
      it ("should when given left brace return " ++
          "ARABIC LETTER ALEF WASLA") $ do
        deromanization '{' `shouldBe`  'ٱ'
      it ("should when given capital P return " ++
          "ARABIC LETTER PEH") $ do
        deromanization 'P' `shouldBe`  'پ'
      it ("should when given capital J return " ++
          "ARABIC LETTER TCHEH") $ do
        deromanization 'J' `shouldBe`  'چ'
      it ("should when given capital V return " ++
          "ARABIC LETTER VEH") $ do
        deromanization 'V' `shouldBe` 'ڤ'
      it ("should when given capital G return " ++
          "ARABIC LETTER GAF") $ do
        deromanization 'G' `shouldBe` 'گ'
      it ("should return space for space") $ do
        deromanization ' ' `shouldBe` ' '
      it ("should return Arabic question mark for question mark") $ do
        deromanization '?' `shouldBe` '؟'
      it ("should return Arabic comma for comma") $ do
        deromanization ',' `shouldBe` '،'
      it ("should return Arabic semicolon for semicolon") $ do
        deromanization ';' `shouldBe` '؛'
