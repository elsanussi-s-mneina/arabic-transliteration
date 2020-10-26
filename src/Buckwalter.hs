module Buckwalter where

romanization :: Char -> Char
romanization '\x0621' = '\'' 
-- ء
-- ARABIC LETTER HAMZA (Unicode: U+0621) becomes APOSTROPHE (Unicode: U+0027).

romanization '\x0622' = '|'
-- آ
-- ARABIC LETTER ALEF WITH MADDA ABOVE (Unicode: U+0622) becomes VERTICAL LINE (Unicode: U+007C).

romanization '\x0623' = '>'
-- أ
-- ARABIC LETTER ALEF WITH HAMZA ABOVE  (Unicode: U+0623) becomes GREATER-THAN SIGN (Unicode: U+003E).

romanization '\x0624' = '&'
-- ؤ
-- ARABIC LETTER WAW WITH HAMZA ABOVE (Unicode: U+0624) becomes AMPERSAND (Unicode: U+0026).

romanization '\x0625' = '<'
-- إ
-- ARABIC LETTER ALEF WITH HAMZA BELOW (Unicode: U+0625) becomes LESS-THAN SIGN (Unicode: U+003C). 

romanization '\x0626' = '}'
-- ئ
-- ARABIC LETTER YEH WITH HAMZA ABOVE (Unicode: U+0626) becomes RIGHT CURLY BRACKET (Unicode: U+007D).

romanization '\x0627' = 'A'
-- ا
-- ARABIC LETTER ALEF (Unicode: U+0627) becomes LATIN CAPITAL LETTER A (Unicode: U+0041).

romanization '\x0628' = 'b'
-- ب
-- ARABIC LETTER BEH (Unicode: U+0628) becomes LATIN SMALL LETTER B (Unicode: U+0062).

romanization '\x0629' = 'p'
-- ة
-- ARABIC LETTER TEH MARBUTA (Unicode: U+0629) becomes LATIN SMALL LETTER P (Unicode: U+0070).

romanization '\x062A' = 't'
-- ت
-- ARABIC LETTER TEH (Unicode: U+062A) becomes LATIN SMALL LETTER T (Unicode: U+0074).

romanization '\x062B' = 'v'
-- ث
-- ARABIC LETTER THEH (Unicode: U+062B) becomes LATIN SMALL LETTER V (Unicode: U+0076).

romanization '\x062C' = 'j'
-- ج
-- ARABIC LETTER JEEM (Unicode: U+062C) becomes LATIN SMALL LETTER J (Unicode: U+006A).

romanization '\x062D' = 'H'
-- ح
-- ARABIC LETTER HAH (Unicode: U+062D) becomes LATIN CAPITAL LETTER H (Unicode: U+0048).

romanization '\x062E' = 'x'
-- خ
-- ARABIC LETTER KHAH (Unicode: U+062E) becomes LATIN SMALL LETTER X (Unicode: U+0078).

romanization '\x062F' = 'd'
-- د
-- ARABIC LETTER DAL (Unicode: U+062F) becomes LATIN SMALL LETTER D (Unicode: U+0064).

romanization '\x0630' = '*'
-- ذ
-- ARABIC LETTER THAL (Unicode: U+0630) becomes ASTERISK (Unicode: U+002A).

romanization '\x0631' = 'r'
-- ر
-- ARABIC LETTER REH (Unicode: U+0631) becomes LATIN SMALL LETTER R (Unicode: U+0072).

romanization '\x0632' = 'z'
-- ز
-- ARABIC LETTER ZAIN (Unicode: U+0632) becomes LATIN SMALL LETTER Z (Unicode: U+007A).

romanization '\x0633' = 's'
-- س
-- ARABIC LETTER SEEN (Unicode: U+0633) becomes LATIN SMALL LETTER S (Unicode: U+0073).
romanization '\x0634' = '$'
-- ش
-- ARABIC LETTER SHEEN (Unicode: U+0634) becomes DOLLAR SIGN (Unicode: U+0024).

romanization '\x0635' = 'S'
-- ص
-- ARABIC LETTER SAD (Unicode: U+0635) becomes LATIN CAPITAL LETTER S (Unicode: U+0053).

romanization '\x0636' = 'D'
-- ض
-- ARABIC LETTER DAD (Unicode: U+0636) becomes LATIN CAPITAL LETTER D (Unicode: U+0044).

romanization '\x0637' = 'T'
-- ط
-- ARABIC LETTER TAH
-- Unicode: U+0637

romanization '\x0638' = 'Z'
-- ظ
-- ARABIC LETTER ZAH
-- Unicode: U+0638

romanization '\x0639' = 'E'
-- ع
-- ARABIC LETTER AIN
-- Unicode: U+0639

romanization '\x063A' = 'g'
-- غ
-- ARABIC LETTER GHAIN
-- Unicode: U+063A

romanization '\x0640' = '_'
-- ـ
-- ARABIC TATWEEL
-- Unicode: U+0640

romanization '\x0641' = 'f'
-- ف
-- ARABIC LETTER FEH
-- Unicode: U+0641

romanization '\x0642' = 'q'
-- ق
-- ARABIC LETTER QAF
-- Unicode: U+0642

romanization '\x0643' = 'k'
-- ك
-- ARABIC LETTER KAF
-- Unicode: U+0643

romanization '\x0644' = 'l'
-- ل
-- ARABIC LETTER LAM
-- Unicode: U+0644

romanization '\x0645' = 'm'
-- م
-- ARABIC LETTER MEEM
-- Unicode: U+0645

romanization '\x0646' = 'n'
-- ن
-- ARABIC LETTER NOON
-- Unicode: U+0646

romanization '\x0647' = 'h'
-- ه
-- ARABIC LETTER HEH
-- Unicode: U+0647

romanization '\x0648' = 'w'
-- و
-- ARABIC LETTER WAW
-- Unicode: U+0648

romanization '\x0649' = 'Y'
-- ى
-- ARABIC LETTER ALEF MAKSURA
-- Unicode: U+0649

romanization '\x064A' = 'y'
-- ي
-- ARABIC LETTER YEH
-- Unicode: U+064A

romanization '\x064B'  = 'F'
-- ً
-- ARABIC FATHATAN
-- Unicode: U+064B

romanization '\x064C'  = 'N'
-- ٌ
-- ARABIC DAMMATAN
-- Unicode: U+064C

romanization '\x064D'  = 'K'
-- ٍ
-- ARABIC KASRATAN
-- Unicode: U+064D

romanization '\x064E'  = 'a'
-- َ
-- ARABIC FATHA
-- Unicode: U+064E

romanization '\x064F'  = 'u'
-- ُ
-- ARABIC DAMMA
-- Unicode: U+064F

romanization '\x0650'  = 'i'
-- ِ
-- ARABIC KASRA
-- Unicode: U+0650

romanization '\x0651'  = '~'
-- ّ
-- ARABIC SHADDA
-- Unicode: U+0651

romanization '\x0652'  = 'o'
-- ْ
-- ARABIC SUKUN
-- Unicode: U+0652

romanization '\x0670'  = '`'
-- ٰ
-- ARABIC LETTER SUPERSCRIPT ALEF
-- Unicode: U+0670

romanization '\x0671' = '{'
-- ٱ
-- ARABIC LETTER ALEF WASLA
-- Unicode: U+0671

romanization '\x067E' = 'P'
-- پ
-- ARABIC LETTER PEH
-- Unicode: U+067E

romanization '\x0686' = 'J'
-- چ
-- ARABIC LETTER TCHEH
-- Unicode: U+0686

romanization '\x06A4' = 'V'
-- ڤ
-- ARABIC LETTER VEH
-- Unicode: U+06A4

romanization '\x06AF' = 'G'
-- گ
-- ARABIC LETTER GAF
-- Unicode: U+06AF

-- The following are not defined in the Buckwalter transliteration system
-- but are needed in practice
romanization '\x061F' = '?'  -- question mark
-- ؟
-- ARABIC QUESTION MARK
-- Unicode: U+061F

romanization '\x060C' = ','  -- comma
-- ،
-- ARABIC COMMA
-- Unicode: U+060C

romanization '\x061B' = ';'  -- semicolon
-- ؛
-- ARABIC SEMICOLON
-- Unicode: U+061B

romanization  x = x



deromanization :: Char -> Char
deromanization '\'' =  '\x0621'
-- ء
-- APOSTROPHE (Unicode: U+0027) becomes ARABIC LETTER HAMZA (Unicode: U+0621).

deromanization '|'  =  '\x0622'
-- آ
-- VERTICAL LINE (Unicode: U+007C) becomes ARABIC LETTER ALEF WITH MADDA ABOVE (Unicode: U+0622).

deromanization '>'  =  '\x0623'
-- أ
-- GREATER-THAN SIGN (Unicode: U+003E) becomes ARABIC LETTER ALEF WITH HAMZA ABOVE  (Unicode: U+0623).

deromanization '&'  =  '\x0624'
-- ؤ
-- AMPERSAND (Unicode: U+0026) becomes ARABIC LETTER WAW WITH HAMZA ABOVE (Unicode: U+0624).

deromanization '<'  =  '\x0625'
-- إ
-- LESS-THAN SIGN (Unicode: U+003C) becomes ARABIC LETTER ALEF WITH HAMZA BELOW (Unicode: U+0625). 

deromanization '}'  =  '\x0626'
-- ئ
-- RIGHT CURLY BRACKET (Unicode: U+007D) becomes ARABIC LETTER YEH WITH HAMZA ABOVE (Unicode: U+0626).

deromanization 'A'  =  '\x0627'
-- ا
-- LATIN CAPITAL LETTER A (Unicode: U+0041) becomes ARABIC LETTER ALEF (Unicode: U+0627).

deromanization 'b'  =  '\x0628'
-- ب
-- ARABIC LETTER BEH
-- Unicode: U+0628

deromanization 'p'  =  '\x0629'
-- ة
-- ARABIC LETTER TEH MARBUTA
-- Unicode: U+0629

deromanization 't'  =  '\x062A'
-- ت
-- ARABIC LETTER TEH
-- Unicode: U+062A

deromanization 'v'  =  'ث'
-- ث
-- ARABIC LETTER THEH
-- Unicode: U+062B

deromanization 'j'  =  '\x062C'
-- ج
-- ARABIC LETTER JEEM
-- Unicode: U+062C

deromanization 'H'  =  '\x062D'
-- ح
-- ARABIC LETTER HAH
-- Unicode: U+062D

deromanization 'x'  =  '\x062E'
-- خ
-- ARABIC LETTER KHAH
-- Unicode: U+062E

deromanization 'd'  =  '\x062F'
-- د
-- ARABIC LETTER DAL
-- Unicode: U+062F

deromanization '*'  =  '\x0630'
-- ذ
-- ARABIC LETTER THAL
-- Unicode: U+0630

deromanization 'r'  =  '\x0631'
-- ر
-- ARABIC LETTER REH
-- Unicode: U+0631

deromanization 'z'  =  '\x0632'
-- ز
-- ARABIC LETTER ZAIN
-- Unicode: U+0632

deromanization 's'  =  '\x0633'
-- س
-- ARABIC LETTER SEEN
-- Unicode: U+0633

deromanization '$'  =  '\x0634'
-- ش
-- ARABIC LETTER SHEEN
-- Unicode: U+0634

deromanization 'S'  =  '\x0635'
-- ص
-- ARABIC LETTER SAD
-- Unicode: U+0635

deromanization 'D'  =  '\x0636'
-- ض
-- ARABIC LETTER DAD
-- Unicode: U+0636

deromanization 'T'  =  '\x0637'
-- ط
-- ARABIC LETTER TAH
-- Unicode: U+0637

deromanization 'Z'  =  '\x0638'
-- ظ
-- ARABIC LETTER ZAH
-- Unicode: U+0638

deromanization 'E'  =  '\x0639'
-- ع
-- ARABIC LETTER AIN
-- Unicode: U+0639

deromanization 'g'  =  '\x063A'
-- غ
-- ARABIC LETTER GHAIN
-- Unicode: U+063A

deromanization '_'  =  '\x0640'
-- ـ
-- ARABIC TATWEEL
-- Unicode: U+0640

deromanization 'f'  =  '\x0641'
-- ف
-- ARABIC LETTER FEH
-- Unicode: U+0641

deromanization 'q'  =  '\x0642'
-- ق
-- ARABIC LETTER QAF
-- Unicode: U+0642

deromanization 'k'  =  '\x0643'
-- ك
-- ARABIC LETTER KAF
-- Unicode: U+0643

deromanization 'l'  =  '\x0644'
-- ل
-- ARABIC LETTER LAM
-- Unicode: U+0644

deromanization 'm'  =  '\x0645'
-- م
-- ARABIC LETTER MEEM
-- Unicode: U+0645

deromanization 'n'  =  '\x0646'
-- ن
-- ARABIC LETTER NOON
-- Unicode: U+0646

deromanization 'h'  =  '\x0647'
-- ه
-- ARABIC LETTER HEH
-- Unicode: U+0647

deromanization 'w'  =  '\x0648'
-- و
-- ARABIC LETTER WAW
-- Unicode: U+0648

deromanization 'Y'  =  '\x0649'
-- ى
-- ARABIC LETTER ALEF MAKSURA
-- Unicode: U+0649

deromanization 'y'  =  '\x064A'
-- ي
-- ARABIC LETTER YEH
-- Unicode: U+064A

deromanization 'F'  =  '\x064B'
-- ً
-- ARABIC FATHATAN
-- Unicode: U+064B

deromanization 'N'  =  '\x064C'
-- ٌ
-- ARABIC DAMMATAN
-- Unicode: U+064C


deromanization 'K'  =  '\x064D'
-- ٍ
-- ARABIC KASRATAN
-- Unicode: U+064D


deromanization 'a'  =  '\x064E'
-- َ
-- ARABIC FATHA
-- Unicode: U+064E

deromanization 'u'  =  '\x064F'
-- ُ
-- ARABIC DAMMA
-- Unicode: U+064F

deromanization 'i'  =  '\x0650'
-- ِ
-- ARABIC KASRA
-- Unicode: U+0650


deromanization '~'  =  '\x0651'
-- ّ
-- ARABIC SHADDA
-- Unicode: U+0651

deromanization 'o'  =  '\x0652'
-- ْ
-- ARABIC SUKUN
-- Unicode: U+0652

deromanization '`'  =  '\x0670'
-- ٰ
-- ARABIC LETTER SUPERSCRIPT ALEF
-- Unicode: U+0670

deromanization '{'  =  '\x0671'
-- ٱ
-- ARABIC LETTER ALEF WASLA
-- Unicode: U+0671


deromanization 'P'  =  '\x067E'
-- پ
-- ARABIC LETTER PEH
-- Unicode: U+067E


deromanization 'J'  =  '\x0686'
-- چ
-- ARABIC LETTER TCHEH
-- Unicode: U+0686


deromanization 'V'  =  '\x06A4'
-- ڤ
-- ARABIC LETTER VEH
-- Unicode: U+06A4


deromanization 'G'  =  '\x06AF'
-- گ
-- ARABIC LETTER GAF
-- Unicode: U+06AF


-- The following are not defined in Buckwalter transliteration
-- but are needed in practice.
deromanization '?'  =  '\x061F'  -- question mark
-- ؟
-- ARABIC QUESTION MARK
-- Unicode: U+061F


deromanization ','  =  '\x060C'  -- comma
-- ،
-- ARABIC COMMA
-- Unicode: U+060C


deromanization ';'  =  '\x061B'  -- semicolon
-- ؛
-- ARABIC SEMICOLON
-- Unicode: U+061B

deromanization  x   =   x
