import System.Environment
import System.Console.GetOpt
import System.IO
import Buckwalter
import ISO233_2

-- How to use:
-- Assuming you have compiled, and produced the executable
-- named
-- arabic-transliteration.

-- To print out the conversion from Latin characters to Arabic
-- characters of the contents of the file hello.txt
-- by using the Buckwalter transliteration system.
-- run the following command.
-- ./arabic-transliteration -a hello.txt

-- to go the opposite way use the -l flag, thus:
-- ./arabic-transliteration -a other.txt

----Command line options----
data Action = BuckwalterLatinToArabic | ArabicToBuckwalterLatin | ISOLatinToArabic | ArabicToISOLatin  deriving (Show, Eq)
data Options =
  Options
  { optHelp :: Bool
  , optAction :: Action
  }
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options
  { optHelp = False
  , optAction = ArabicToBuckwalterLatin
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['l'] ["artola"]
    (NoArg (\ opts -> opts { optAction = ArabicToBuckwalterLatin}))
    "Arabic to Latin (Buckwalter transliteration system)"
  , Option ['a'] ["latoar"]
    (NoArg (\ opts -> opts { optAction = BuckwalterLatinToArabic}))
    "Latin to Arabic (Buckwalter transliteration system)"
  , Option ['i'] ["artolaiso"]
    (NoArg (\ opts -> opts { optAction = ArabicToISOLatin}))
    "Latin to Arabic (ISO 232-2 transliteration system)"
  , Option ['s'] ["latoariso"]
    (NoArg (\ opts -> opts { optAction = ISOLatinToArabic}))
    "Latin to Arabic (ISO 232-2 transliteration system)"
  ]

translitOptions :: [String] -> IO (Options, [String])
translitOptions argv =
  case getOpt Permute options argv of
    (options, nonOptions, []    ) -> return (foldl (flip id) defaultOptions options, nonOptions)
    (_,       _,          errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where header = "Usage: exe [OPTION...] filename"

chooseTranslitFunc :: Action -> (Char -> Char)
chooseTranslitFunc BuckwalterLatinToArabic = deromanization
chooseTranslitFunc ArabicToBuckwalterLatin = romanization
chooseTranslitFunc ISOLatinToArabic = deromanization_iso232
chooseTranslitFunc ArabicToISOLatin = romanization_iso232


main :: IO ()
main =
  do
  argv <- getArgs
  (opts, fname) <- translitOptions argv
  let translitFunc = chooseTranslitFunc (optAction opts)
  if length fname == 1
  then do
      putStrLn ("The file name is " ++ (concat fname))
      handle <- openFile (fname !! 0) ReadMode
      contents <- hGetContents handle
      putStrLn (map translitFunc contents)
      hClose handle
  else do
      putStrLn "Starting console mode. Enter a line and it will be transliterated..."
      contents <- getLine
      putStrLn (map translitFunc contents)
  putStrLn "Program terminated normally."
