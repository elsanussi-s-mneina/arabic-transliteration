module ISO233_2 where

romanization_iso232 '\x0621' = '\x02BE'
-- ء
-- ARABIC LETTER HAMZA
-- Unicode: U+0621

-- ʾ
-- MODIFIER LETTER RIGHT HALF RING
-- Unicode: U+02BE

romanization_iso232 '\x0628' = '\x0062'
-- ب
-- ARABIC LETTER BEH
-- Unicode: U+0628

-- b
-- LATIN SMALL LETTER B
-- Unicode: U+0062

romanization_iso232 '\x062A' = '\x0074'
-- ت
-- ARABIC LETTER TEH
-- Unicode: U+062A

-- t
-- LATIN SMALL LETTER T
-- Unicode: U+0074

romanization_iso232 '\x062B' = '\x1E6F'
-- ث
-- ARABIC LETTER THEH
-- Unicode: U+062B

-- ṯ
-- LATIN SMALL LETTER T WITH LINE BELOW
-- Unicode: U+1E6F

romanization_iso232 '\x062C' = '\x01E7'
-- ج
-- ARABIC LETTER JEEM
-- Unicode: U+062C

-- ǧ
-- LATIN SMALL LETTER G WITH CARON
-- Unicode: U+01E7

romanization_iso232 '\x062D' = '\x1E25'
-- ح
-- ARABIC LETTER HAH
-- Unicode: U+062D

-- ḥ
-- LATIN SMALL LETTER H WITH DOT BELOW
-- Unicode: U+1E25

romanization_iso232 '\x062E' = '\x1E96'
-- خ
-- ARABIC LETTER KHAH
-- Unicode: U+062E

-- ẖ
-- LATIN SMALL LETTER H WITH LINE BELOW
-- Unicode: U+1E96

romanization_iso232 '\x062F' = '\x0064'
-- د
-- ARABIC LETTER DAL
-- Unicode: U+062F

romanization_iso232 '\x0630' = '\x1E0F'
-- ذ
-- ARABIC LETTER THAL
-- Unicode: U+0630

romanization_iso232 '\x0631' = '\x0072'
-- ر
-- ARABIC LETTER REH
-- Unicode: U+0631

romanization_iso232 '\x0632' = '\x007A'
-- ز
-- ARABIC LETTER ZAIN
-- Unicode: U+0632

romanization_iso232 '\x0633' = '\x0073'
-- س
-- ARABIC LETTER SEEN
-- Unicode: U+0633

romanization_iso232 '\x0634' = '\x0161'
-- ش
-- ARABIC LETTER SHEEN
-- Unicode: U+0634

-- š
-- LATIN SMALL LETTER S WITH CARON
-- Unicode: U+0161

romanization_iso232 '\x0635' = '\x1E63'
-- ص
-- ARABIC LETTER SAD
-- Unicode: U+0635

-- ṣ
-- LATIN SMALL LETTER S WITH DOT BELOW
-- Unicode: U+1E63

romanization_iso232 '\x0636' = '\x1E0D'
-- ض
-- ARABIC LETTER DAD
-- Unicode: U+0636

-- ḍ
-- LATIN SMALL LETTER D WITH DOT BELOW
-- Unicode: U+1E0D

romanization_iso232 '\x0637' = '\x1E6D'
-- ط
-- ARABIC LETTER TAH
-- Unicode: U+0637

-- ṭ
-- LATIN SMALL LETTER T WITH DOT BELOW
-- Unicode: U+1E6D

romanization_iso232 '\x0638' = '\x1E93'
-- ظ
-- ARABIC LETTER ZAH
-- Unicode: U+0638

-- ẓ
-- LATIN SMALL LETTER Z WITH DOT BELOW
-- Unicode: U+1E93

romanization_iso232 '\x0639' = '\x02BF'
-- ع
-- ARABIC LETTER AIN
-- Unicode: U+0639

-- ʿ
-- MODIFIER LETTER LEFT HALF RING
-- Unicode: U+02BF

romanization_iso232 '\x063A' = '\x0121'
-- غ
-- ARABIC LETTER GHAIN
-- Unicode: U+063A

-- ġ
-- LATIN SMALL LETTER G WITH DOT ABOVE
-- Unicode: U+0121

romanization_iso232 '\x0641' = '\x0066'
-- ف
-- ARABIC LETTER FEH
-- Unicode: U+0641

romanization_iso232 '\x0642' = '\x0071'
-- ق
-- ARABIC LETTER QAF
-- Unicode: U+0642

romanization_iso232 '\x0643' = '\x006B'
-- ك
-- ARABIC LETTER KAF
-- Unicode: U+0643

romanization_iso232 '\x0644' = '\x006C'
-- ل
-- ARABIC LETTER LAM
-- Unicode: U+0644

romanization_iso232 '\x0645' = '\x006D'
-- م
-- ARABIC LETTER MEEM
-- Unicode: U+0645

romanization_iso232 '\x0646' = '\x006E'
-- ن
-- ARABIC LETTER NOON
-- Unicode: U+0646

romanization_iso232 '\x0647' = '\x0068'
-- ه
-- ARABIC LETTER HEH
-- Unicode: U+0647

romanization_iso232 '\x0629' = '\x1E97'
-- ة
-- ARABIC LETTER TEH MARBUTA
-- Unicode: U+0629

-- ẗ
-- LATIN SMALL LETTER T WITH DIAERESIS
-- Unicode: U+1E97

romanization_iso232 '\x0648' = '\x0077'
-- و
-- ARABIC LETTER WAW
-- Unicode: U+0648

romanization_iso232 '\x064A' = '\x0079'
-- ي
-- ARABIC LETTER YEH
-- Unicode: U+064A

romanization_iso232 x = x





deromanization_iso232 '\x02BE' = '\x0621'
deromanization_iso232 '\x0062' = '\x0628'
deromanization_iso232 '\x0074' = '\x062A'
deromanization_iso232 '\x1E6F' = '\x062B'
deromanization_iso232 '\x01E7' = '\x062C'
deromanization_iso232 '\x1E25' = '\x062D'
deromanization_iso232 '\x1E96' = '\x062E'
deromanization_iso232 '\x0064' = '\x062F'
deromanization_iso232 '\x1E0F' = '\x0630'
deromanization_iso232 '\x0072' = '\x0631'
deromanization_iso232 '\x007A' = '\x0632'
deromanization_iso232 '\x0073' = '\x0633'
deromanization_iso232 '\x0161' = '\x0634'
deromanization_iso232 '\x1E63' = '\x0635'
deromanization_iso232 '\x1E0D' = '\x0636'
deromanization_iso232 '\x1E6D' = '\x0637'
deromanization_iso232 '\x1E93' = '\x0638'
deromanization_iso232 '\x02BF' = '\x0639'
deromanization_iso232 '\x0121' = '\x063A'
deromanization_iso232 '\x0066' = '\x0641'
deromanization_iso232 '\x0071' = '\x0642'
deromanization_iso232 '\x006B' = '\x0643'
deromanization_iso232 '\x006C' = '\x0644'
deromanization_iso232 '\x006D' = '\x0645'
deromanization_iso232 '\x006E' = '\x0646'
deromanization_iso232 '\x0068' = '\x0647'
deromanization_iso232 '\x1E97' = '\x0629'
deromanization_iso232 '\x0077' = '\x0648'
deromanization_iso232 '\x0079' = '\x064A'
deromanization_iso232 x = x