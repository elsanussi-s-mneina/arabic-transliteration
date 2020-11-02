module BuckwalterTests (tests) where

import Buckwalter

import LittleTest (Test(Test))

makeTest :: String -> Char -> Char -> Test
makeTest testName arabicChar latinChar = Test (testName, romanization arabicChar == latinChar && arabicChar == deromanization latinChar)

hamzaSpec = makeTest "APOSTROPHE (U+0027) for ARABIC LETTER HAMZA (U+0621)" '\x0621' '\x0027'
alefWithMaddaSpec = makeTest "VERTICAL LINE (U+007C) for ARABIC LETTER ALEF WITH MADDA ABOVE (U+0622)" '\x0622' '\x007C'
alefWithHamzaBelowSpec = makeTest "LESS-THAN SIGN (U+003C) for ARABIC LETTER ALEF WITH HAMZA BELOW (U+0625)" '\x0625' '\x003C'
alefWithHamzaAboveSpec = makeTest "GREATER-THAN SIGN (U+003E) for ARABIC LETTER ALEF WITH HAMZA ABOVE (U+0623)" '\x0623' '\x003E'
wawWithHamzaAboveSpec = makeTest "AMPERSAND (U+0026) for ARABIC LETTER WAW WITH HAMZA ABOVE (U+0624)" '\x0624' '\x0026'
yehWithHamzaAboveSpec = makeTest "RIGHT CURLY BRACKET (U+007D) for ARABIC LETTER YEH WITH HAMZA ABOVE (U+0626)" '\x0626' '\x007D'
alefSpec = makeTest "LATIN CAPITAL LETTER A (U+0041) for ARABIC LETTER ALEF (U+0627)" '\x0627' '\x0041'
behSpec = makeTest "LATIN SMALL LETTER B (U+0062) for ARABIC LETTER BEH (U+0628)" '\x0628' '\x0062'
tehMarbutaSpec = makeTest "LATIN SMALL LETTER P (U+0070) for ARABIC LETTER TEH MARBUTA (U+0629)" '\x0629' '\x0070'
tehSpec = makeTest "LATIN SMALL LETTER T (U+0074) for ARABIC LETTER TEH (U+062A)" '\x062A' '\x0074'
thehSpec = makeTest "LATIN SMALL LETTER V (U+0076) for ARABIC LETTER THEH (U+062B)" '\x062B' '\x0076'
jeemSpec = makeTest "LATIN SMALL LETTER J (U+006A) for ARABIC LETTER JEEM (U+062C)" '\x062C' '\x006A'
hahSpec = makeTest "LATIN CAPITAL LETTER H (U+0048) for ARABIC LETTER HAH" '\x062D' 'H'
khahSpec = makeTest "LATIN SMALL LETTER X (U+0078) for ARABIC LETTER KHAH" '\x062E' 'x'
dalSpec = makeTest "LATIN SMALL LETTER D (U+0064) for ARABIC LETTER DAL" '\x062F' 'd'
thalSpec = makeTest "ASTERISK (U+002A) for ARABIC LETTER THAL" '\x0630' '*'
rehSpec = makeTest "LATIN SMALL LETTER R (U+0072) for ARABIC LETTER REH" '\x0631' 'r'
zainSpec = makeTest "LATIN SMALL LETTER Z (U+007A) for ARABIC LETTER ZAIN" '\x0632' 'z'
seenSpec = makeTest "LATIN SMALL LETTER S (U+0073) for ARABIC LETTER SEEN" '\x0633' 's'
sheenSpec = makeTest "DOLLAR SIGN (U+0024) for ARABIC LETTER SHEEN" '\x0634' '$'
sadSpec = makeTest "LATIN CAPITAL LETTER S (U+0053) for ARABIC LETTER SAD" '\x0635' 'S'
dadSpec = makeTest "LATIN CAPITAL LETTER D (U+0044) for ARABIC LETTER DAD" '\x0636' 'D'
tahSpec = makeTest "LATIN CAPITAL LETTER T (U+0054) for ARABIC LETTER TAH" '\x0637' 'T'
zahSpec = makeTest "LATIN CAPITAL LETTER Z (U+005A) for ARABIC LETTER ZAH" '\x0638' 'Z'
ainSpec = makeTest "LATIN CAPITAL LETTER E (U+0045) for ARABIC LETTER AIN" '\x0639' 'E'
ghainSpec = makeTest "LATIN SMALL LETTER G (U+0067) for ARABIC LETTER GHAIN" '\x063A' 'g'
tatweelSpec = makeTest "LOW LINE (U+005F) for ARABIC TATWEEL" '\x0640' '_'
fehSpec = makeTest "LATIN SMALL LETTER F (U+0066) for ARABIC LETTER FEH" '\x0641' 'f'
qafSpec = makeTest "LATIN SMALL LETTER Q (U+0071) for ARABIC LETTER QAF" '\x0642' 'q'
kafSpec = makeTest "LATIN SMALL LETTER K (U+006B) for ARABIC LETTER KAF" '\x0643' 'k'
lamSpec = makeTest "LATIN SMALL LETTER L (U+006C) for ARABIC LETTER LAM" '\x0644' 'l'
meemSpec = makeTest "LATIN SMALL LETTER M (U+006D) for ARABIC LETTER MEEM" '\x0645' 'm'
noonSpec = makeTest "LATIN SMALL LETTER N (U+006E) for ARABIC LETTER NOON" '\x0646' 'n'
hehSpec = makeTest "LATIN SMALL LETTER H (U+0068) for ARABIC LETTER HEH" '\x0647' 'h'
wawSpec = makeTest "LATIN SMALL LETTER W (U+0077) for ARABIC LETTER WAW" '\x0648' 'w'
alefMaksuraSpec = makeTest "LATIN CAPITAL LETTER Y (U+0059) for ARABIC LETTER ALEF MAKSURA" '\x0649' 'Y'
yehSpec =  makeTest "LATIN SMALL LETTER Y (U+0079) for ARABIC LETTER YEH" '\x064A' 'y'
fathatanSpec = makeTest "LATIN CAPITAL LETTER F (U+0046) for ARABIC FATHATAN" '\x064B' 'F'
dammatanSpec = makeTest "LATIN CAPITAL LETTER N (U+004E) for ARABIC DAMMATAN" '\x064C' 'N'
kasratanSpec = makeTest "LATIN CAPITAL LETTER K (U+004B) for ARABIC KASRATAN" '\x064D' 'K'
fathaSpec = makeTest "LATIN SMALL LETTER A (U+0061) for ARABIC FATHA" '\x064E' 'a'
dammaSpec = makeTest "LATIN SMALL LETTER U (U+0075) for ARABIC DAMMA" '\x064F' 'u'
kasraSpec = makeTest "LATIN SMALL LETTER I (U+0069) for ARABIC KASRA" '\x0650' 'i'
shaddaSpec = makeTest "TILDE (U+007E) for ARABIC SHADDA" '\x0651' '~'
sukunSpec = makeTest "LATIN SMALL LETTER O (U+006F) for ARABIC SUKUN" '\x0652' 'o'
superscriptAlefSpec = makeTest "GRAVE ACCENT (U+0060) for ARABIC LETTER SUPERSCRIPT ALEF" '\x0670' '`'
alefWaslaSpec = makeTest "LEFT CURLY BRACKET (U+007B) for ARABIC LETTER ALEF WASLA" '\x0671'  '{'
pehSpec = makeTest "LATIN CAPITAL LETTER P (U+0050) for ARABIC LETTER PEH" '\x067E' 'P'
tchehSpec = makeTest "LATIN CAPITAL LETTER J (U+004A) for ARABIC LETTER TCHEH" '\x0686' 'J'
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


  
