module ISO233_2 where

-- Note to developer we keep the left hand side of each
-- "arrow" expression on a separate line from the right hand side
-- due to difficulties working with right to left text
-- in an editor.
romanization_iso232 char = case char of
  'ء'
     -> 'ʾ'
  'ب'
     -> 'b'
  'ت'
     -> 't'
  'ث'
     -> 'ṯ'
  'ج'
     -> 'ǧ'
  'ح'
     -> 'ḥ'
  'خ'
     -> 'ẖ'
  'د'
     -> 'd'
  'ذ'
     -> 'ḏ'
  'ر'
     -> 'r'
  'ز'
     -> 'z'
  'س'
     -> 's'
  'ش'
     -> 'š'