import System.Environment
import System.Console.GetOpt
import System.IO
import Buckwalter

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
data Action = BuckwalterToArabic | BuckwalterToLatin deriving (Show, Eq)
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
  , optAction = BuckwalterToLatin
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['l'] ["artola"]
    (NoArg (\ opts -> opts { optAction = BuckwalterToLatin}))
    "Arabic to Latin (Buckwalter translitertion system)"
  , Option ['a'] ["latoar"]
    (NoArg (\ opts -> opts { optAction = BuckwalterToArabic}))
    "Latin to Arabic (Buckwalter translitertion system)"
  ]

translitOptions :: [String] -> IO (Options, [String])
translitOptions argv =
  case getOpt Permute options argv of
    (options, nonOptions, []    ) -> return (foldl (flip id) defaultOptions options, nonOptions)
    (_,       _,          errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where header = "Usage: exe [OPTION...] filename"

main :: IO ()
main =
  do
  argv <- getArgs
  (opts, fname) <- translitOptions argv
  let translitFunc = if BuckwalterToArabic == optAction opts then deromanization else romanization
  if length fname == 1
  then do
      putStrLn ("The file name is " ++ (concat fname))
      handle <- openFile (fname !! 0) ReadMode
      contents <- hGetContents handle
      putStrLn (map translitFunc contents)
      hClose handle
  else do
      putStrLn "Starting console mode..."
      contents <- getLine
      putStrLn (map translitFunc contents)
  putStrLn "Program terminated normally."
