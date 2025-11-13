module Main where

import Options.Applicative
import System.IO (hIsTerminalDevice, stdout)

import CorrectUnicorn
import Config

main :: IO ()
main = do
  sysConfig <- loadSystemConfig
  userConfig <- loadUserConfig
  cliSettings <- execParser opts

  if settingsInteractive cliSettings
    then showDictionaryStatus sysConfig (settingsDictPath cliSettings)
  else if settingsSecurity cliSettings
    then showSecurityStats sysConfig userConfig cliSettings
  else generateAndDisplay sysConfig userConfig cliSettings
  where
    opts = info (settings <**> helper)
      ( fullDesc
     <> progDesc "passphrase generator inspired by xkcd 936"
     <> header "have fun!" )

generateAndDisplay :: SystemConfig -> UserConfig -> Settings -> IO ()
generateAndDisplay sysConfig userConfig cliSettings = do
  isTTY <- hIsTerminalDevice stdout
  let noColor = settingsNoColor cliSettings || not isTTY
  runtimeConfig <- mergeConfig
    sysConfig
    userConfig
    (settingsWordCount cliSettings)
    (settingsDictPath cliSettings)
    (settingsSeparator cliSettings)
    noColor
    (settingsMinChars cliSettings)
    (settingsCapitalize cliSettings)
  genPassword runtimeConfig
