{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( SystemConfig(..)
  , UserConfig(..)
  , RuntimeConfig(..)
  , loadSystemConfig
  , loadUserConfig
  , mergeConfig
  , defaultSystemConfig
  , defaultUserConfig
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Toml
import Toml (TomlCodec, (.=))
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)

-- | System-wide configuration (security settings)
data SystemConfig = SystemConfig
  { dictPaths :: [Text]
  , minWords :: Int
  , minWordLength :: Int
  , substitutions :: Map Text Text
  } deriving (Show, Eq)

-- | User configuration (UI preferences only)
data UserConfig = UserConfig
  { colors :: [Text]
  , separator :: Text
  , bold :: Bool
  } deriving (Show, Eq)

-- | Runtime configuration (merged from all sources)
data RuntimeConfig = RuntimeConfig
  { runtimeDictPath :: Text
  , runtimeWordCount :: Int
  , runtimeColors :: [Text]
  , runtimeSeparator :: Text
  , runtimeBold :: Bool
  , runtimeSubstitutions :: Map Text Text
  , runtimeMinWords :: Int
  , runtimeMinWordLength :: Int
  } deriving (Show, Eq)

-- | Default system configuration
defaultSystemConfig :: SystemConfig
defaultSystemConfig = SystemConfig
  { dictPaths = ["/usr/share/dict/english", "/usr/share/dict/american-english", "/usr/share/dict/words"]
  , minWords = 1000
  , minWordLength = 2
  , substitutions = Map.fromList [("o", "0"), ("a", "@"), ("e", "3"), ("i", "!"), ("s", "$")]
  }

-- | Default user configuration
defaultUserConfig :: UserConfig
defaultUserConfig = UserConfig
  { colors = ["blue", "yellow"]
  , separator = ""
  , bold = True
  }

-- | Tomland codec for SystemConfig
systemConfigCodec :: TomlCodec SystemConfig
systemConfigCodec = SystemConfig
  <$> Toml.arrayOf Toml._Text "dict-paths" .= dictPaths
  <*> Toml.int "min-words" .= minWords
  <*> Toml.int "min-word-length" .= minWordLength
  <*> Toml.tableMap Toml._KeyText Toml.text "substitutions" .= substitutions

-- | Tomland codec for UserConfig
userConfigCodec :: TomlCodec UserConfig
userConfigCodec = UserConfig
  <$> Toml.arrayOf Toml._Text "colors" .= colors
  <*> Toml.text "separator" .= separator
  <*> Toml.bool "bold" .= bold

-- | Load system configuration from /etc/correct-unicorn/config.toml
loadSystemConfig :: IO SystemConfig
loadSystemConfig = do
  let systemConfigPath = "/etc/correct-unicorn/config.toml"
  exists <- doesFileExist systemConfigPath
  if exists
    then do
      result <- Toml.decodeFileEither systemConfigCodec systemConfigPath
      case result of
        Right config -> return config
        Left _ -> return defaultSystemConfig
    else return defaultSystemConfig
  `catch` \(_ :: SomeException) -> return defaultSystemConfig

-- | Load user configuration from ~/.config/correct-unicorn/config.toml
loadUserConfig :: IO UserConfig
loadUserConfig = do
  homeDir <- getHomeDirectory
  let userConfigPath = homeDir </> ".config" </> "correct-unicorn" </> "config.toml"
  exists <- doesFileExist userConfigPath
  if exists
    then do
      result <- Toml.decodeFileEither userConfigCodec userConfigPath
      case result of
        Right config -> return config
        Left _ -> return defaultUserConfig
    else return defaultUserConfig
  `catch` \(_ :: SomeException) -> return defaultUserConfig


-- | Merge configurations with CLI settings
mergeConfig :: SystemConfig -> UserConfig -> Int -> Maybe FilePath -> RuntimeConfig
mergeConfig sysConfig userConfig wordCount mDictPath =
  RuntimeConfig
    { runtimeDictPath = case mDictPath of
        Just path -> T.pack path
        Nothing -> case dictPaths sysConfig of
          (p:_) -> p
          [] -> "/usr/share/dict/english"
    , runtimeWordCount = wordCount
    , runtimeColors = colors userConfig
    , runtimeSeparator = separator userConfig
    , runtimeBold = bold userConfig
    , runtimeSubstitutions = substitutions sysConfig
    , runtimeMinWords = minWords sysConfig
    , runtimeMinWordLength = minWordLength sysConfig
    }
