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
behSpec = makeTest "LATIN SMALL LETTER B (b) for ARABIC LETTER BEH" '\x0628' 'b'
tehSpec = makeTest "LATIN SMALL LETTER T (t) for ARABIC LETTER TEH" '\x062A' 't'
thehSpec = makeTest "LATIN SMALL LETTER T WITH LINE BELOW for ARABIC LETTER THEH" 'ث' 'ṯ'

jeemSpec = makeTest "LATIN SMALL LETTER G WITH CARON for ARABIC LETTER JEEM" 'ج' 'ǧ'

hahSpec = makeTest "LATIN SMALL LETTER H WITH DOT BELOW for ARABIC LETTER HAH" 'ح' 'ḥ'

khahSpec = makeTest "LATIN SMALL LETTER H WITH LINE BELOW for ARABIC LETTER KHAH" 'خ' 'ẖ'

dalSpec = makeTest "LATIN SMALL LETTER D for ARABIC LETTER DAL" 'د' 'd'
thalSpec = makeTest "LATIN SMALL LETTER D WITH LINE BELOW for ARABIC LETTER THAL" 'ذ' 'ḏ'
rehSpec = makeTest "LATIN SMALL LETTER R for ARABIC LETTER REH" 'ر' 'r'

zainSpec = makeTest "LATIN SMALL LETTER Z for ARABIC LETTER ZAIN" 'ز' 'z'
seenSpec = makeTest "LATIN SMALL LETTER S for ARABIC LETTER SEEN" 'س' 's'
sheenSpec = makeTest "LATIN SMALL LETTER S WITH CARON for ARABIC LETTER SHEEN" 'ش' 'š'

sadSpec = makeTest "LATIN SMALL LETTER S WITH DOT BELOW for ARABIC LETTER SAD" 'ص' 'ṣ'
dadSpec = makeTest "LATIN SMALL LETTER D WITH DOT BELOW for ARABIC LETTER DAD" 'ض' 'ḍ'
tahSpec = makeTest "LATIN SMALL LETTER T WITH DOT BELOW for ARABIC LETTER TAH" 'ط' 'ṭ'
zahSpec = makeTest "LATIN SMALL LETTER Z WITH DOT BELOW for ARABIC LETTER ZAH" 'ظ' 'ẓ'
ainSpec = makeTest "MODIFIER LETTER LEFT HALF RING for ARABIC LETTER AIN" 'ع' 'ʿ'
ghainSpec = makeTest "LATIN SMALL LETTER G WITH DOT ABOVE for ARABIC LETTER GHAIN" 'غ' 'ġ'
fehSpec = makeTest "LATIN SMALL LETTER F for ARABIC LETTER FEH"  'ف' 'f'
qafSpec = makeTest "LATIN SMALL LETTER Q for ARABIC LETTER QAF" 'ق' 'q'
kafSpec = makeTest "LATIN SMALL LETTER K for ARABIC LETTER KAF" 'ك' 'k'
lamSpec = makeTest "LATIN SMALL LETTER L for ARABIC LETTER LAM" 'ل' 'l'
meemSpec = makeTest "LATIN SMALL LETTER M for ARABIC LETTER MEEM" 'م' 'm'
noonSpec = makeTest "LATIN SMALL LETTER N for ARABIC LETTER NOON" 'ن' 'n'
hehSpec = makeTest "LATIN SMALL LETTER H for ARABIC LETTER HEH" 'ه' 'h'
tehMarbutaSpec = makeTest "LATIN SMALL LETTER T WITH DIAERESIS for ARABIC LETTER TEH MARBUTA" 'ة' 'ẗ'
wawSpec = makeTest "LATIN SMALL LETTER W for ARABIC LETTER WAW" 'و' 'w'
yehSpec = makeTest "LATIN SMALL LETTER Y for ARABIC LETTER YEH" 'ي' 'y'

tests :: [Test]
tests = 
  [ hamzaSpec, behSpec, tehSpec, thehSpec, jeemSpec, hahSpec, khahSpec, dalSpec, thalSpec, rehSpec, zainSpec, seenSpec, sheenSpec, sadSpec, dadSpec, tahSpec, zahSpec, ainSpec, ghainSpec, fehSpec, qafSpec, kafSpec, lamSpec, meemSpec, noonSpec, hehSpec, tehMarbutaSpec, wawSpec, yehSpec]
