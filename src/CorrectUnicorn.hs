module CorrectUnicorn
    ( genPassword
    , Settings(..)
    , settings
    ) where

import Options.Applicative
import System.Random
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char

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

-- | Apply transformation only if condition is true, otherwise identity
applyIf :: Bool -> (a -> a) -> (a -> a)
applyIf True f = f
applyIf False _ = id

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

      -- Word-level transformations
      wordTransforms = [ applyIf (runtimeCapitalize config) capitalizeWords
                       , applyIf (not $ runtimeNoColor config) paintWords'
                       ]
      applyWordTransforms = foldr (.) id wordTransforms

      -- String-level transformations (after joining)
      sep = T.unpack $ runtimeSeparator config
      stringTransforms = [ applyIf (runtimeBold config) PrettyAnsi.bold ]
      applyStringTransforms = foldr (.) id stringTransforms

  in applyStringTransforms . joinWithSeparator sep . applyWordTransforms $ usedWords

genPassword :: RuntimeConfig -> IO ()
genPassword config = do
  content <- readFile (T.unpack $ runtimeDictPath config)
  stdGen <- getStdGen
  let dictionaryWords = lines content
      output = generatePassword config dictionaryWords (runtimeWordCount config) stdGen
  putStrLn output
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

resolveColors :: [Text] -> [String]
resolveColors colorNames =
  let resolved = mapMaybe (colorNameToAnsi . T.unpack) colorNames
  in if null resolved then [ansiBlue, ansiYellow] else resolved

joinWithSeparator :: String -> [String] -> String
joinWithSeparator _ [] = ""
joinWithSeparator _ [x] = x
joinWithSeparator sep (x:xs) = x ++ sep ++ joinWithSeparator sep xs
