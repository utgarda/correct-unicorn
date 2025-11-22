module CorrectUnicorn
    ( genPassword
    , genPasswordString
    , showDictionaryStatus
    , showSecurityStats
    , Settings(..)
    , settings
    , calculateEntropy
    , calculateKeyspace
    , estimateCrackTime
    , stripAnsi
    , capitalizeWords
    , joinWithSeparator
    , resolveColors
    , generateWithWordCount
    , filterWords
    , filterWordsByLength
    , defaultStopChars
    ) where

import Options.Applicative
import System.Random
import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char
import Text.Printf (printf)
import Data.Duration (humanReadableDuration')

import qualified PrettyAnsi
import PrettyAnsi (ansiBlue, ansiYellow, colorNameToAnsi, paintWords)
import Config

data Settings = Settings
  { settingsWordCount :: Int
  , settingsDictPath :: Maybe FilePath
  , settingsSeparator :: Maybe String
  , settingsNoColor :: Bool
  , settingsMinChars :: Maybe Int
  , settingsCapitalize :: Bool
  , settingsInteractive :: Bool
  , settingsSecurity :: Bool
  , settingsClipboard :: Bool
  , settingsQuiet :: Bool
  } deriving (Show, Eq)

settings :: Parser Settings
settings = Settings
      <$> option auto
          ( long "words"
         <> short 'w'
         <> metavar "COUNT"
         <> showDefault
         <> value 4
         <> help "Number of words to use" )
      <*> optional (strOption
          ( long "dict"
         <> metavar "PATH"
         <> help "Dictionary file path override" ))
      <*> optional (strOption
          ( long "separator"
         <> short 's'
         <> metavar "SEP"
         <> help "Word separator (default: space)" ))
      <*> switch
          ( long "no-color"
         <> short 'p'
         <> help "Disable ANSI colors" )
      <*> optional (option auto
          ( long "chars"
         <> short 'c'
         <> metavar "COUNT"
         <> help "Minimum total character count (excluding ANSI codes)" ))
      <*> switch
          ( long "capitalize"
         <> help "Capitalize first letter of each word" )
      <*> switch
          ( long "interactive"
         <> short 'I'
         <> help "Show available dictionaries and their status" )
      <*> switch
          ( long "security"
         <> short 'z'
         <> help "Show security statistics (entropy and crack time estimates)" )
      <*> switch
          ( long "clipboard"
         <> short 'x'
         <> help "Copy passphrase to clipboard" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Suppress output to stdout (use with --clipboard)" )

-- | Dictionary source type
data DictSource = Custom | Configured | Fallback deriving (Show, Eq)

-- | Dictionary status for display
data DictStatus = DictStatus
  { dictPath :: FilePath
  , dictExists :: Bool
  , dictWordCount :: Maybe Int
  , dictSource :: DictSource
  } deriving (Show, Eq)

-- | Apply transformation only if condition is true, otherwise identity
applyIf :: Bool -> (a -> a) -> (a -> a)
applyIf True f = f
applyIf False _ = id

-- | Default characters to filter from dictionary words
defaultStopChars :: [Char]
defaultStopChars = "'"

-- | Filter out words containing any of the specified characters
filterWords :: [Char] -> [String] -> [String]
filterWords stopChars = filter (\word -> not (any (`elem` word) stopChars))

-- | Filter words by length (min and max)
filterWordsByLength :: Int -> Int -> [String] -> [String]
filterWordsByLength minLen maxLen = filter (\word -> let len = length word in len >= minLen && len <= maxLen)

-- | Capitalize first letter of each word in a list
capitalizeWords :: [String] -> [String]
capitalizeWords = map capitalizeWord
  where
    capitalizeWord [] = []
    capitalizeWord (c:cs) = Data.Char.toUpper c : cs

-- | Strip ANSI escape codes from a string
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':rest) =
  case dropWhile (/= 'm') rest of
    (_:remaining) -> stripAnsi remaining  -- Skip the 'm' and continue
    [] -> []  -- End of string
stripAnsi (c:rest) = c : stripAnsi rest

-- | Generate a password with a specific word count
generateWithWordCount :: RuntimeConfig -> [String] -> Int -> StdGen -> String
generateWithWordCount config dictionaryWords wordCount stdGen =
  let dictSize = length dictionaryWords
      indexes = take wordCount $ randoms stdGen :: [Int]
      cappedIndexes = fmap (`mod` dictSize) indexes
      usedWords = [dictionaryWords !! x | x <- cappedIndexes]

      -- Prepare word-level transformations
      ansiColors = resolveColors (runtimeColors config)
      paintWords' ws = paintWords ws ansiColors

      -- Word-level transformations (applied in order: capitalize first, then color)
      wordTransforms = [ applyIf (not $ runtimeNoColor config) paintWords'
                       , applyIf (runtimeCapitalize config) capitalizeWords
                       ]
      applyWordTransforms = foldr (.) id wordTransforms

      -- String-level transformations (after joining)
      sep = T.unpack $ runtimeSeparator config
      stringTransforms = [ applyIf (runtimeBold config) PrettyAnsi.bold ]
      applyStringTransforms = foldr (.) id stringTransforms

  in applyStringTransforms . joinWithSeparator sep . applyWordTransforms $ usedWords

-- | Determine which dictionary will be selected
determineSelectedDict :: [Text] -> Maybe FilePath -> IO FilePath
determineSelectedDict configPaths mCustomPath = case mCustomPath of
  Just path -> return path
  Nothing -> do
    found <- findValidDictionary configPaths
    return $ maybe "/usr/share/dict/words" T.unpack found

-- | Check a single dictionary and return its status
checkDict :: FilePath -> DictSource -> Int -> Int -> IO DictStatus
checkDict path source minLen maxLen = do
  exists <- doesFileExist path
  wordCount <- if exists
    then do
      content <- readFile path
      let filteredWords = filterWordsByLength minLen maxLen $ filterWords defaultStopChars $ lines content
      return $ Just $ length filteredWords
    else return Nothing
  return $ DictStatus path exists wordCount source

-- | Display dictionary discovery status
showDictionaryStatus :: SystemConfig -> Maybe FilePath -> IO ()
showDictionaryStatus sysConfig mDictPath = do
  putStrLn "Available dictionaries:"
  putStrLn ""

  -- Build list of paths to check
  let configPaths = dictPaths sysConfig
      fallbackPath = "/usr/share/dict/words"
      hasFallback = any ((== fallbackPath) . T.unpack) configPaths
      pathsToCheck = case mDictPath of
        Just customPath -> [(customPath, Custom)]
        Nothing -> map (\p -> (T.unpack p, Configured)) configPaths
                   ++ [(fallbackPath, Fallback) | not hasFallback]

  -- Check all dictionaries
  let minLen = minWordLength sysConfig
      maxLen = maxWordLength sysConfig
  statuses <- mapM (\(path, source) -> checkDict path source minLen maxLen) pathsToCheck

  -- Determine which will be selected
  selectedPath <- determineSelectedDict configPaths mDictPath

  -- Display each dictionary
  mapM_ (printDictStatus selectedPath) statuses

  -- Display selection summary
  putStrLn ""
  let selectedStatus = filter (\s -> dictPath s == selectedPath) statuses
  case selectedStatus of
    (s:_) -> case dictWordCount s of
      Just wc -> printf "Using: %s (%d words)\n" (dictPath s) wc
      Nothing -> printf "Using: %s (word count unavailable)\n" (dictPath s)
    [] -> printf "Using: %s\n" selectedPath

  where
    printDictStatus :: FilePath -> DictStatus -> IO ()
    printDictStatus selectedPath status =
      let isSelected = dictPath status == selectedPath
          marker = if dictExists status then "✓" else "✗"
          selectedTag = if isSelected then " [SELECTED]" else ""
          sourceTag = case dictSource status of
            Custom -> " [CUSTOM]"
            Fallback -> " [FALLBACK]"
            Configured -> ""
          countStr = case dictWordCount status of
            Just wc -> printf " (%d words)" wc
            Nothing -> " (not found)"
      in printf "  %s %s%s%s%s\n" marker (dictPath status) countStr sourceTag selectedTag

-- | Calculate bits of entropy: log2(dictSize ^ wordCount)
calculateEntropy :: Int -> Int -> Double
calculateEntropy dictSize wordCount =
  logBase 2 (fromIntegral dictSize ** fromIntegral wordCount)

-- | Calculate total keyspace: dictSize ^ wordCount
calculateKeyspace :: Int -> Int -> Integer
calculateKeyspace dictSize wordCount =
  toInteger dictSize ^ wordCount

-- | Calculate crack time in seconds
estimateCrackTime :: Integer -> Double -> Double
estimateCrackTime keyspace guessesPerSec =
  fromIntegral keyspace / guessesPerSec

-- | Display security statistics
showSecurityStats :: SystemConfig -> UserConfig -> Settings -> IO ()
showSecurityStats sysConfig _userConfig cliSettings = do
  -- Determine which dictionary will be used
  selectedDictPath <- determineSelectedDict (dictPaths sysConfig) (settingsDictPath cliSettings)

  -- Read and count words
  content <- readFile selectedDictPath
  let allWords = lines content
      originalSize = length allWords
      minLen = minWordLength sysConfig
      maxLen = maxWordLength sysConfig

      -- Apply filters separately to track counts
      afterApostropheFilter = filterWords defaultStopChars allWords
      apostropheFilteredCount = originalSize - length afterApostropheFilter

      tooShort = filter (\w -> length w < minLen) afterApostropheFilter
      tooShortCount = length tooShort

      tooLong = filter (\w -> length w > maxLen) afterApostropheFilter
      tooLongCount = length tooLong

      dictWords = filterWordsByLength minLen maxLen afterApostropheFilter
      dictSize = length dictWords

      wordCount = settingsWordCount cliSettings
      keyspace = calculateKeyspace dictSize wordCount
      entropy = calculateEntropy dictSize wordCount

  -- Display statistics header
  printf "Security Statistics:\n\n\
         \  Dictionary: %s\n\
         \  Original size: %d words\n\n\
         \  Filtering applied:\n\
         \    - Removed apostrophes ('): %d words\n\
         \    - Too short (< %d chars): %d words\n\
         \    - Too long (> %d chars): %d words\n\n\
         \  Usable words: %d\n\
         \  Words in passphrase: %d\n\
         \  Total combinations: %d\n\
         \  Entropy: %.1f bits\n\n\
         \Estimated crack time (knowing dictionary and word count):\n"
    selectedDictPath originalSize apostropheFilteredCount
    minLen tooShortCount maxLen tooLongCount
    dictSize wordCount keyspace entropy

  -- Show crack times at different rates
  let rates = [ (1000000, "1,000,000 guesses/second (modern GPU)")
              , (1000, "1,000 guesses/second (online throttled)")
              , (0.2, "1 guess per 5 seconds (strict limit)")
              ]
  mapM_ (showCrackTime keyspace) rates

  where
    showCrackTime :: Integer -> (Double, String) -> IO ()
    showCrackTime keyspace (rate, desc) = do
      let seconds = estimateCrackTime keyspace rate
          timeStr = humanReadableDuration' seconds
      printf "  At %s: %s\n" desc timeStr

genPasswordString :: RuntimeConfig -> IO String
genPasswordString config = do
  content <- readFile (T.unpack $ runtimeDictPath config)
  stdGen <- getStdGen
  let minLen = runtimeMinWordLength config
      maxLen = runtimeMaxWordLength config
      dictionaryWords = filterWordsByLength minLen maxLen $ filterWords defaultStopChars $ lines content
  return $ generatePassword config dictionaryWords (runtimeWordCount config) stdGen
  where
    generatePassword :: RuntimeConfig -> [String] -> Int -> StdGen -> String
    generatePassword cfg dictionaryWords wordCount gen =
      let result = generateWithWordCount cfg dictionaryWords wordCount gen
          charCount = length $ stripAnsi result
      in case runtimeMinChars cfg of
           Nothing -> result
           Just minChars ->
             if charCount >= minChars
               then result
               else generatePassword cfg dictionaryWords (wordCount + 1) gen

genPassword :: RuntimeConfig -> IO ()
genPassword config = genPasswordString config >>= putStrLn

resolveColors :: [Text] -> [String]
resolveColors colorNames =
  let resolved = mapMaybe (colorNameToAnsi . T.unpack) colorNames
  in if null resolved then [ansiBlue, ansiYellow] else resolved

joinWithSeparator :: String -> [String] -> String
joinWithSeparator _ [] = ""
joinWithSeparator _ [x] = x
joinWithSeparator sep (x:xs) = x ++ sep ++ joinWithSeparator sep xs
