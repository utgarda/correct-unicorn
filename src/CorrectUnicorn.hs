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

import qualified PrettyAnsi
import PrettyAnsi (ansiBlue, ansiYellow, colorNameToAnsi, paintWords)
import Config

data Settings = Settings
  { settingsWordCount :: Int
  , settingsDictPath :: Maybe FilePath
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

genPassword :: RuntimeConfig -> IO ()
genPassword config = do
  content <- readFile (T.unpack $ runtimeDictPath config)
  stdGen <- getStdGen
  let dictionaryWords = lines content
      dictSize = length dictionaryWords
      wordCount = runtimeWordCount config
      indexes = take wordCount $ randoms stdGen :: [Int]
      cappedIndexes = fmap (`mod` dictSize) indexes
      usedWords = [dictionaryWords !! x | x <- cappedIndexes]
      ansiColors = resolveColors (runtimeColors config)
      coloredWords = paintWords usedWords ansiColors
      sep = T.unpack $ runtimeSeparator config
      output = joinWithSeparator sep coloredWords
      finalOutput = if runtimeBold config then PrettyAnsi.bold output else output
  putStrLn finalOutput

resolveColors :: [Text] -> [String]
resolveColors colorNames =
  let resolved = mapMaybe (colorNameToAnsi . T.unpack) colorNames
  in if null resolved then [ansiBlue, ansiYellow] else resolved

joinWithSeparator :: String -> [String] -> String
joinWithSeparator _ [] = ""
joinWithSeparator _ [x] = x
joinWithSeparator sep (x:xs) = x ++ sep ++ joinWithSeparator sep xs
