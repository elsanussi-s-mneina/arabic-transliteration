module ISO233_2 where

romanization_iso232 '\x0621' = 'ʾ'
-- ء
-- ARABIC LETTER HAMZA
-- Unicode: U+0621

romanization_iso232 '\x0628' = 'b'
-- ب
-- ARABIC LETTER BEH
-- Unicode: U+0628

romanization_iso232 '\x062A' = 't'
-- ت
-- ARABIC LETTER TEH
-- Unicode: U+062A

romanization_iso232 '\x062B' = 'ṯ'
-- ث
-- ARABIC LETTER THEH
-- Unicode: U+062B

romanization_iso232 '\x062C' = 'ǧ'
-- ج
-- ARABIC LETTER JEEM
-- Unicode: U+062C

romanization_iso232 '\x062D' = 'ḥ'
-- ح
-- ARABIC LETTER HAH
-- Unicode: U+062D

romanization_iso232 '\x062E' = 'ẖ'
-- خ
-- ARABIC LETTER KHAH
-- Unicode: U+062E

romanization_iso232 '\x062F' = 'd'
-- د
-- ARABIC LETTER DAL
-- Unicode: U+062F

romanization_iso232 '\x0630' = 'ḏ'
-- ذ
-- ARABIC LETTER THAL
-- Unicode: U+0630

romanization_iso232 '\x0631' = 'r'
-- ر
-- ARABIC LETTER REH
-- Unicode: U+0631

romanization_iso232 '\x0632' = 'z'
-- ز
-- ARABIC LETTER ZAIN
-- Unicode: U+0632

romanization_iso232 '\x0633' = 's'
-- س
-- ARABIC LETTER SEEN
-- Unicode: U+0633

romanization_iso232 '\x0634' = 'š'
-- ش
-- ARABIC LETTER SHEEN
-- Unicode: U+0634

romanization_iso232 '\x0635' = 'ṣ'
-- ص
-- ARABIC LETTER SAD
-- Unicode: U+0635

romanization_iso232 '\x0636' = 'ḍ'
-- ض
-- ARABIC LETTER DAD
-- Unicode: U+0636

romanization_iso232 '\x0637' = 'ṭ'
-- ط
-- ARABIC LETTER TAH
-- Unicode: U+0637

romanization_iso232 '\x0638' = 'ẓ'
-- ظ
-- ARABIC LETTER ZAH
-- Unicode: U+0638

romanization_iso232 '\x0639' = 'ʿ'
-- ع
-- ARABIC LETTER AIN
-- Unicode: U+0639

romanization_iso232 '\x063A' = 'ġ'
-- غ
-- ARABIC LETTER GHAIN
-- Unicode: U+063A

romanization_iso232 '\x0641' = 'f'
-- ف
-- ARABIC LETTER FEH
-- Unicode: U+0641

romanization_iso232 '\x0642' = 'q'
-- ق
-- ARABIC LETTER QAF
-- Unicode: U+0642

romanization_iso232 '\x0643' = 'k'
-- ك
-- ARABIC LETTER KAF
-- Unicode: U+0643

romanization_iso232 '\x0644' = 'l'
-- ل
-- ARABIC LETTER LAM
-- Unicode: U+0644

romanization_iso232 '\x0645' = 'm'
-- م
-- ARABIC LETTER MEEM
-- Unicode: U+0645

romanization_iso232 '\x0646' = 'n'
-- ن
-- ARABIC LETTER NOON
-- Unicode: U+0646

romanization_iso232 '\x0647' = 'h'
-- ه
-- ARABIC LETTER HEH
-- Unicode: U+0647

romanization_iso232 '\x0629' = 'ẗ'
-- ة
-- ARABIC LETTER TEH MARBUTA
-- Unicode: U+0629

romanization_iso232 '\x0648' = 'w'
-- و
-- ARABIC LETTER WAW
-- Unicode: U+0648

romanization_iso232 '\x064A' = 'y'
-- ي
-- ARABIC LETTER YEH
-- Unicode: U+064A

romanization_iso232 x = x





deromanization_iso232 'ʾ' = '\x0621'
deromanization_iso232 'b' = '\x0628'
deromanization_iso232 't' = '\x062A'
deromanization_iso232 'ṯ' = '\x062B'
deromanization_iso232 'ǧ' = '\x062C'
deromanization_iso232 'ḥ' = '\x062D'
deromanization_iso232 'ẖ' = '\x062E'
deromanization_iso232 'd' = '\x062F'
deromanization_iso232 'ḏ' = '\x0630'
deromanization_iso232 'r' = '\x0631'
deromanization_iso232 'z' = '\x0632'
deromanization_iso232 's' = '\x0633'
deromanization_iso232 'š' = '\x0634'
deromanization_iso232 'ṣ' = '\x0635'
deromanization_iso232 'ḍ' = '\x0636'
deromanization_iso232 'ṭ' = '\x0637'
deromanization_iso232 'ẓ' = '\x0638'
deromanization_iso232 'ʿ' = '\x0639'
deromanization_iso232 'ġ' = '\x063A'
deromanization_iso232 'f' = '\x0641'
deromanization_iso232 'q' = '\x0642'
deromanization_iso232 'k' = '\x0643'
deromanization_iso232 'l' = '\x0644'
deromanization_iso232 'm' = '\x0645'
deromanization_iso232 'n' = '\x0646'
deromanization_iso232 'h' = '\x0647'
deromanization_iso232 'ẗ' = '\x0629'
deromanization_iso232 'w' = '\x0648'
deromanization_iso232 'y' = '\x064A'
deromanization_iso232 x = x