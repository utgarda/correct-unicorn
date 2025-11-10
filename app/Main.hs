module Main where

import Options.Applicative

import CorrectUnicorn
import Config

main :: IO ()
main = do
  sysConfig <- loadSystemConfig
  userConfig <- loadUserConfig
  cliSettings <- execParser opts
  let runtimeConfig = mergeConfig
        sysConfig
        userConfig
        (settingsWordCount cliSettings)
        (settingsDictPath cliSettings)
  genPassword runtimeConfig
  where
    opts = info (settings <**> helper)
      ( fullDesc
     <> progDesc "passphrase generator inspired by xkcd 936"
     <> header "have fun!" )
