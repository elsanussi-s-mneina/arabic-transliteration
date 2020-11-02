-- Implementation of the transliteration system:
-- named ISO 233-2.
module ISO233_2Tests where






import ISO233_2
-- For names of Latin characters: https://unicode.org/charts/PDF/U0000.pdf
-- For names of Arabic characters: http://unicode.org/charts/PDF/U0600.pdf

import LittleTest (Test(Test))

makeTest :: String -> Char -> Char -> Test
makeTest testName arabicChar latinChar = Test (testName, romanization_iso232 arabicChar == latinChar && arabicChar == deromanization_iso232 latinChar)


hamzaSpec = makeTest "MODIFIER LETTER RIGHT HALF RING (U+02BE) for ARABIC LETTER HAMZA (U+0621)" '\x0621' '\x02BE'
behSpec = makeTest "LATIN SMALL LETTER B (U+0062) for ARABIC LETTER BEH (U+0628)" '\x0628' 'b'
tehSpec = makeTest "LATIN SMALL LETTER T (U+0074) for ARABIC LETTER TEH (U+062A)" '\x062A' 't'
thehSpec = makeTest "LATIN SMALL LETTER T WITH LINE BELOW (U+1E6F) for ARABIC LETTER THEH (U+062B)" 'ث' 'ṯ'

jeemSpec = makeTest "LATIN SMALL LETTER G WITH CARON (U+01E7) for ARABIC LETTER JEEM (U+062C)" 'ج' 'ǧ'

hahSpec = makeTest "LATIN SMALL LETTER H WITH DOT BELOW (U+1E25) for ARABIC LETTER HAH (U+062D)" 'ح' 'ḥ'

khahSpec = makeTest "LATIN SMALL LETTER H WITH LINE BELOW (U+1E96) for ARABIC LETTER KHAH (U+062E)" 'خ' 'ẖ'

dalSpec = makeTest "LATIN SMALL LETTER D (U+0064) for ARABIC LETTER DAL (U+062F)" 'د' 'd'
thalSpec = makeTest "LATIN SMALL LETTER D WITH LINE BELOW (U+1E0F) for ARABIC LETTER THAL (U+0630)" 'ذ' 'ḏ'
rehSpec = makeTest "LATIN SMALL LETTER R (U+0072) for ARABIC LETTER REH (U+0631)" 'ر' 'r'

zainSpec = makeTest "LATIN SMALL LETTER Z (U+007A) for ARABIC LETTER ZAIN (U+0632)" 'ز' 'z'
seenSpec = makeTest "LATIN SMALL LETTER S (U+0073) for ARABIC LETTER SEEN (U+0633)" 'س' 's'
sheenSpec = makeTest "LATIN SMALL LETTER S WITH CARON (U+0161) for ARABIC LETTER SHEEN (U+0634)" 'ش' 'š'

sadSpec = makeTest "LATIN SMALL LETTER S WITH DOT BELOW (U+1E63) for ARABIC LETTER SAD (U+0635)" 'ص' 'ṣ'
dadSpec = makeTest "LATIN SMALL LETTER D WITH DOT BELOW (U+1E0D) for ARABIC LETTER DAD (U+0636)" 'ض' 'ḍ'
tahSpec = makeTest "LATIN SMALL LETTER T WITH DOT BELOW (U+1E6D) for ARABIC LETTER TAH (U+0637)" 'ط' 'ṭ'
zahSpec = makeTest "LATIN SMALL LETTER Z WITH DOT BELOW (U+1E93) for ARABIC LETTER ZAH (U+0638)" 'ظ' 'ẓ'
ainSpec = makeTest "MODIFIER LETTER LEFT HALF RING (U+02BF) for ARABIC LETTER AIN (U+0639)" 'ع' 'ʿ'
ghainSpec = makeTest "LATIN SMALL LETTER G WITH DOT ABOVE (U+0121) for ARABIC LETTER GHAIN (U+063A)" 'غ' 'ġ'
fehSpec = makeTest "LATIN SMALL LETTER F (U+0066) for ARABIC LETTER FEH (U+0641)"  'ف' 'f'
qafSpec = makeTest "LATIN SMALL LETTER Q (U+0071) for ARABIC LETTER QAF (U+0642)" 'ق' 'q'
kafSpec = makeTest "LATIN SMALL LETTER K (U+006B) for ARABIC LETTER KAF (U+0643)" 'ك' 'k'
lamSpec = makeTest "LATIN SMALL LETTER L (U+006C) for ARABIC LETTER LAM (U+0644)" 'ل' 'l'
meemSpec = makeTest "LATIN SMALL LETTER M (U+006D) for ARABIC LETTER MEEM (U+0645)" 'م' 'm'
noonSpec = makeTest "LATIN SMALL LETTER N (U+006E) for ARABIC LETTER NOON (U+0646)" 'ن' 'n'
hehSpec = makeTest "LATIN SMALL LETTER H (U+0068) for ARABIC LETTER HEH (U+0647)" 'ه' 'h'
tehMarbutaSpec = makeTest "LATIN SMALL LETTER T (U+1E97) WITH DIAERESIS for ARABIC LETTER TEH MARBUTA (U+0629)" 'ة' 'ẗ'
wawSpec = makeTest "LATIN SMALL LETTER W (U+0077) for ARABIC LETTER WAW (U+0648)" 'و' 'w'
yehSpec = makeTest "LATIN SMALL LETTER Y (U+0079) for ARABIC LETTER YEH (U+064A)" 'ي' 'y'

tests :: [Test]
tests = 
  [ hamzaSpec, behSpec, tehSpec, thehSpec, jeemSpec, hahSpec, khahSpec, dalSpec, thalSpec, rehSpec, zainSpec, seenSpec, sheenSpec, sadSpec, dadSpec, tahSpec, zahSpec, ainSpec, ghainSpec, fehSpec, qafSpec, kafSpec, lamSpec, meemSpec, noonSpec, hehSpec, tehMarbutaSpec, wawSpec, yehSpec]
