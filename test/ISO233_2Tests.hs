-- Implementation of the transliteration system:
-- named ISO 233-2.
module ISO233_2Tests where






import ISO233_2
-- For names of Latin characters: https://unicode.org/charts/PDF/U0000.pdf
-- For names of Arabic characters: http://unicode.org/charts/PDF/U0600.pdf

import LittleTest (Test(Test))

makeTest :: String -> Char -> Char -> Test
makeTest testName arabicChar latinChar = Test (testName, romanization_iso232 arabicChar == latinChar && arabicChar == deromanization_iso232 latinChar)


hamzaSpec = makeTest "MODIFIER VERTICAL LINE (U+02C8) for ARABIC LETTER HAMZA (U+0621)" '\x0621' '\x02C8'
alefSpec = makeTest "MODIFIER LETTER RIGHT HALF RING (U+02BE) for ARABIC LETTER ALEF (U+0627)" '\x0627' '\x02BE'
behSpec = makeTest "LATIN SMALL LETTER B (U+0062) for ARABIC LETTER BEH (U+0628)" '\x0628' '\x0062'
tehSpec = makeTest "LATIN SMALL LETTER T (U+0074) for ARABIC LETTER TEH (U+062A)" '\x062A' '\x0074'
thehSpec = makeTest "LATIN SMALL LETTER T WITH LINE BELOW (U+1E6F) for ARABIC LETTER THEH (U+062B)" '\x062B' '\x1E6F'

jeemSpec = makeTest "LATIN SMALL LETTER G WITH CARON (U+01E7) for ARABIC LETTER JEEM (U+062C)" '\x062C' '\x01E7'

hahSpec = makeTest "LATIN SMALL LETTER H WITH DOT BELOW (U+1E25) for ARABIC LETTER HAH (U+062D)" '\x062D' '\x1E25'

khahSpec = makeTest "LATIN SMALL LETTER H WITH LINE BELOW (U+1E96) for ARABIC LETTER KHAH (U+062E)" '\x062E' '\x1E96'

dalSpec = makeTest "LATIN SMALL LETTER D (U+0064) for ARABIC LETTER DAL (U+062F)" '\x062F' '\x0064'
thalSpec = makeTest "LATIN SMALL LETTER D WITH LINE BELOW (U+1E0F) for ARABIC LETTER THAL (U+0630)" '\x0630' '\x1E0F'
rehSpec = makeTest "LATIN SMALL LETTER R (U+0072) for ARABIC LETTER REH (U+0631)" '\x0631' '\x0072'

zainSpec = makeTest "LATIN SMALL LETTER Z (U+007A) for ARABIC LETTER ZAIN (U+0632)" '\x0632' '\x007A'
seenSpec = makeTest "LATIN SMALL LETTER S (U+0073) for ARABIC LETTER SEEN (U+0633)" '\x0633' '\x0073'
sheenSpec = makeTest "LATIN SMALL LETTER S WITH CARON (U+0161) for ARABIC LETTER SHEEN (U+0634)" '\x0634' '\x0161'

sadSpec = makeTest "LATIN SMALL LETTER S WITH DOT BELOW (U+1E63) for ARABIC LETTER SAD (U+0635)" '\x0635' '\x1E63'
dadSpec = makeTest "LATIN SMALL LETTER D WITH DOT BELOW (U+1E0D) for ARABIC LETTER DAD (U+0636)" '\x0636' '\x1E0D'
tahSpec = makeTest "LATIN SMALL LETTER T WITH DOT BELOW (U+1E6D) for ARABIC LETTER TAH (U+0637)" '\x0637' '\x1E6D'
zahSpec = makeTest "LATIN SMALL LETTER Z WITH DOT BELOW (U+1E93) for ARABIC LETTER ZAH (U+0638)" '\x0638' '\x1E93'
ainSpec = makeTest "MODIFIER LETTER LEFT HALF RING (U+02BF) for ARABIC LETTER AIN (U+0639)" '\x0639' '\x02BF'
ghainSpec = makeTest "LATIN SMALL LETTER G WITH DOT ABOVE (U+0121) for ARABIC LETTER GHAIN (U+063A)" '\x063A' '\x0121'
fehSpec = makeTest "LATIN SMALL LETTER F (U+0066) for ARABIC LETTER FEH (U+0641)"  '\x0641' '\x0066'
qafSpec = makeTest "LATIN SMALL LETTER Q (U+0071) for ARABIC LETTER QAF (U+0642)" '\x0642' '\x0071'
kafSpec = makeTest "LATIN SMALL LETTER K (U+006B) for ARABIC LETTER KAF (U+0643)" '\x0643' '\x006B'
lamSpec = makeTest "LATIN SMALL LETTER L (U+006C) for ARABIC LETTER LAM (U+0644)" '\x0644' '\x006C'
meemSpec = makeTest "LATIN SMALL LETTER M (U+006D) for ARABIC LETTER MEEM (U+0645)" '\x0645' '\x006D'
noonSpec = makeTest "LATIN SMALL LETTER N (U+006E) for ARABIC LETTER NOON (U+0646)" '\x0646' '\x006E'
hehSpec = makeTest "LATIN SMALL LETTER H (U+0068) for ARABIC LETTER HEH (U+0647)" '\x0647' '\x0068'
tehMarbutaSpec = makeTest "LATIN SMALL LETTER T (U+1E97) WITH DIAERESIS for ARABIC LETTER TEH MARBUTA (U+0629)" '\x0629' '\x1E97'
wawSpec = makeTest "LATIN SMALL LETTER W (U+0077) for ARABIC LETTER WAW (U+0648)" '\x0648' '\x0077'
alefMaksuraSpec = makeTest "LATIN SMALL LETTER Y WITH GRAVE (U+1EF3) for ARABIC LETTER ALEF MAKSURA (U+0649)" '\x0649' '\x1EF3'
yehSpec = makeTest "LATIN SMALL LETTER Y (U+0079) for ARABIC LETTER YEH (U+064A)" '\x064A' '\x0079'


fathatanSpec = makeTest "LATIN SMALL LETTER A WITH ACUTE (U+00E1) for ARABIC FATHATAN (U+064B)" '\x064B' '\x00E1'
dammatanSpec = makeTest "LATIN CAPITAL LETTER U WITH ACUTE (U+00FA) for ARABIC DAMMATAN (U+064C)" '\x064C' '\x00FA'
kasratanSpec = makeTest "LATIN CAPITAL LETTER I WITH ACUTE (U+00ED) for ARABIC KASRATAN (U+064D)" '\x064D' '\x00ED'
fathaSpec = makeTest "LATIN SMALL LETTER A (U+0061) for ARABIC FATHA (U+064E)" '\x064E' '\x0061'
dammaSpec = makeTest "LATIN SMALL LETTER U (U+0075) for ARABIC DAMMA (U+064F)" '\x064F' '\x0075'
kasraSpec = makeTest "LATIN SMALL LETTER I (U+0069) for ARABIC KASRA (U+0650)" '\x0650' '\x0069'
shaddaSpec = makeTest "MACRON (U+00AF) for ARABIC SHADDA (U+0651)" '\x0651' '\x00AF'
sukunSpec = makeTest "RING ABOVE (U+02DA) for ARABIC SUKUN (U+0652)" '\x0652' '\x02DA'
superscriptAlefSpec = makeTest "LATIN SMALL LETTER A WITH CIRCUMFLEX (U+00E2) for ARABIC LETTER SUPERSCRIPT ALEF (U+0670)" '\x0670' '\x00E2'



tests :: [Test]
tests = 
  [ hamzaSpec, alefSpec, behSpec, tehSpec, thehSpec, jeemSpec, hahSpec, khahSpec, dalSpec, thalSpec, rehSpec, zainSpec, seenSpec, sheenSpec, sadSpec, dadSpec, tahSpec, zahSpec, ainSpec, ghainSpec, fehSpec, qafSpec, kafSpec, lamSpec, meemSpec, noonSpec, hehSpec, tehMarbutaSpec, wawSpec, alefMaksuraSpec, yehSpec, fathatanSpec, dammatanSpec, kasratanSpec, fathaSpec, dammaSpec, kasraSpec, shaddaSpec, sukunSpec, superscriptAlefSpec]
