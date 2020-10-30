module BuckwalterTests (tests) where

import Buckwalter

import LittleTest (Test(Test))

makeTest :: String -> Char -> Char -> Test
makeTest testName arabicChar latinChar = Test (testName, romanization arabicChar == latinChar && arabicChar == deromanization latinChar)

hamzaSpec = makeTest "APOSTROPHE (U+0027) for ARABIC LETTER HAMZA (U+0621)" '\x0621' '\x0027'
alefWithMaddaSpec = makeTest "VERTICAL LINE (U+007C) for ARABIC LETTER ALEF WITH MADDA ABOVE (U+0622)" '\x0622' '\x007C'
alefWithHamzaBelowSpec = makeTest "a LESS-THAN SIGN (U+003C) for ARABIC LETTER ALEF WITH HAMZA BELOW (U+0625)" '\x0625' '\x003C'
alefWithHamzaAboveSpec = makeTest "GREATER-THAN SIGN (U+003E) for ARABIC LETTER ALEF WITH HAMZA ABOVE (U+0623)" '\x0623' '\x003E'
wawWithHamzaAboveSpec = makeTest "AMPERSAND (U+0026) for ARABIC LETTER WAW WITH HAMZA ABOVE (U+0624)" '\x0624' '\x0026'
yehWithHamzaAboveSpec = makeTest "a RIGHT CURLY BRACE (U+007D) for ARABIC LETTER YEH WITH HAMZA ABOVE (U+0626)" '\x0626' '\x007D'
alefSpec = makeTest "LATIN CAPITAL LETTER A (U+0041) for ARABIC LETTER ALEF (U+0627)" '\x0627' '\x0041'
behSpec = makeTest "LATIN SMALL LETTER B (U+0062) for ARABIC LETTER BEH (U+0628)" '\x0628' '\x0062'
tehMarbutaSpec = makeTest "LATIN SMALL LETTER P (U+0070) for ARABIC LETTER TEH MARBUTA (U+0629)" '\x0629' '\x0070'
tehSpec = makeTest "LATIN SMALL LETTER T (U+0074) for ARABIC LETTER TEH (U+062A)" '\x062A' '\x0074'
thehSpec = makeTest "LATIN SMALL LETTER V (U+0076) for ARABIC LETTER THEH (U+062B)" '\x062B' '\x0076'
jeemSpec = makeTest "LATIN SMALL LETTER J (U+006A) for ARABIC LETTER JEEM (U+062C)" '\x062C' '\x006A'
hahSpec = makeTest "an uppercase H for ARABIC LETTER HAH" '\x062D' 'H'
khahSpec = makeTest "a lowercase x for ARABIC LETTER KHAH" '\x062E' 'x'
dalSpec = makeTest "a lowercase d for ARABIC LETTER DAL" '\x062F' 'd'
thalSpec = makeTest "an asterisk for ARABIC LETTER THAL" '\x0630' '*'
rehSpec = makeTest "a lowercase r for ARABIC LETTER REH" '\x0631' 'r'
zainSpec = makeTest "a lowercase z for ARABIC LETTER ZAIN" '\x0632' 'z'
seenSpec = makeTest "a lowercase s for ARABIC LETTER SEEN" '\x0633' 's'
sheenSpec = makeTest "a dollar sign $ for ARABIC LETTER SHEEN" '\x0634' '$'
sadSpec = makeTest "a capital S for ARABIC LETTER SAD" '\x0635' 'S'
dadSpec = makeTest "a capital D for ARABIC LETTER DAD" '\x0636' 'D'
tahSpec = makeTest "a capital T for ARABIC LETTER TAH" '\x0637' 'T'
zahSpec = makeTest "a capital Z for ARABIC LETTER ZAH" '\x0638' 'Z'
ainSpec = makeTest "a capital E for ARABIC LETTER AIN" '\x0639' 'E'
ghainSpec = makeTest "a lowercase g for ARABIC LETTER GHAIN" '\x063A' 'g'
tatweelSpec = makeTest "an underscore _ for ARABIC TATWEEL" '\x0640' '_'
fehSpec = makeTest "a lowercase f for ARABIC LETTER FEH" '\x0641' 'f'
qafSpec = makeTest "a lowercase q for ARABIC LETTER QAF" '\x0642' 'q'
kafSpec = makeTest "a lowercase k for ARABIC LETTER KAF" '\x0643' 'k'
lamSpec = makeTest "a lowercase l for ARABIC LETTER LAM" '\x0644' 'l'
meemSpec = makeTest "a lowercase m for ARABIC LETTER MEEM" '\x0645' 'm'
noonSpec = makeTest "a lowercase n for ARABIC LETTER NOON" '\x0646' 'n'
hehSpec = makeTest "a lowercase h for ARABIC LETTER HEH" '\x0647' 'h'
wawSpec = makeTest "a lowercase w for ARABIC LETTER WAW" '\x0648' 'w'
alefMaksuraSpec = makeTest "a capital Y for ARABIC LETTER ALEF MAKSURA" '\x0649' 'Y'
yehSpec =  makeTest "a lowercase y for ARABIC LETTER YEH" '\x064A' 'y'
fathatanSpec = makeTest "a capital F for ARABIC FATHATAN" '\x064B' 'F'
dammatanSpec = makeTest "a capital N for ARABIC DAMMATAN" '\x064C' 'N'
kasratanSpec = makeTest "a capital K for ARABIC KASRATAN" '\x064D' 'K'
fathaSpec = makeTest "a lowercase a for ARABIC FATHA" '\x064E' 'a'
dammaSpec = makeTest "a lowercase u for ARABIC DAMMA" '\x064F' 'u'
kasraSpec = makeTest "a lowercase i for ARABIC KASRA" '\x0650' 'i'
shaddaSpec = makeTest "tilde for ARABIC SHADDA" '\x0651' '~'
sukunSpec = makeTest "o for ARABIC SUKUN" '\x0652' 'o'
superscriptAlefSpec = makeTest "backtick for ARABIC LETTER SUPERSCRIPT ALEF" '\x0670' '`'
alefWaslaSpec = makeTest "left brace for ARABIC LETTER ALEF WASLA" '\x0671'  '{'
pehSpec = makeTest "capital P for ARABIC LETTER PEH" '\x067E' 'P'
tchehSpec = makeTest "capital J for ARABIC LETTER TCHEH" '\x0686' 'J'
vehSpec = makeTest "LATIN CAPITAL LETTER V (U+0056) for ARABIC LETTER VEH (U+06A4)" '\x06A4' '\x0056'
gafSpec = makeTest "LATIN CAPITAL LETTER G (U+0047) for ARABIC LETTER GAF (U+06AF)" '\x06AF' '\x0047'
spaceSpec = makeTest "SPACE (U+0020) for SPACE (U+0020)" '\x0020' '\x0020'
questionMarkSpec = makeTest "QUESTION MARK (U+003F) for ARABIC QUESTION MARK (U+061F)" '\x061F' '\x003F'
commaSpec = makeTest "COMMA (U+002C) for ARABIC COMMA (U+060C)" '\x060C' '\x002C'
semicolonSpec = makeTest "SEMICOLON (U+003B) for ARABIC SEMICOLON (U+061B)" '\x061B' '\x003B'


tests :: [Test]
tests = 
  [ hamzaSpec
  , alefWithMaddaSpec
  , alefWithHamzaBelowSpec
  , alefWithHamzaAboveSpec
  , wawWithHamzaAboveSpec
  , alefWithHamzaBelowSpec
  , yehWithHamzaAboveSpec
  , alefSpec, behSpec, tehMarbutaSpec, tehSpec, thehSpec, jeemSpec, hahSpec, khahSpec, dalSpec, thalSpec, rehSpec, zainSpec, seenSpec, sheenSpec, sadSpec, dadSpec, tahSpec, zahSpec, ainSpec, ghainSpec, tatweelSpec, fehSpec, qafSpec, kafSpec, lamSpec, meemSpec, noonSpec, hehSpec, wawSpec, alefMaksuraSpec, yehSpec, 
    fathatanSpec, dammatanSpec, kasratanSpec, fathaSpec, dammaSpec, kasraSpec, shaddaSpec, sukunSpec,
    superscriptAlefSpec, alefWaslaSpec, pehSpec, tchehSpec, vehSpec, gafSpec, spaceSpec, questionMarkSpec, commaSpec, semicolonSpec
  ]


  
