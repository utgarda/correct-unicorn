module Main where

import Options.Applicative
import System.IO (hIsTerminalDevice, stdout, hPutStrLn, stderr)
import System.Exit (exitFailure)
import Control.Monad (unless)

import CorrectUnicorn
import Config
import qualified PassIntegration as Pass

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

  -- Generate passphrase
  passphrase <- genPasswordString runtimeConfig

  -- Handle pass integration if requested
  case settingsPass cliSettings of
    Nothing ->
      -- Normal output: print to stdout unless quiet mode
      unless (settingsQuiet cliSettings) $ putStrLn passphrase

    Just passPath -> do
      -- Strip ANSI codes for pass storage
      let cleanPassphrase = stripAnsi passphrase

      -- Insert into pass
      result <- Pass.insertIntoPass passPath cleanPassphrase (settingsPassForce cliSettings)

      case result of
        Left err -> do
          -- Print error message to stderr
          hPutStrLn stderr $ Pass.formatPassError err
          exitFailure

        Right () -> do
          -- Success: optionally print to stdout and stderr message
          unless (settingsQuiet cliSettings) $ do
            putStrLn passphrase
            hPutStrLn stderr $ "Passphrase inserted into pass: " ++ passPath
