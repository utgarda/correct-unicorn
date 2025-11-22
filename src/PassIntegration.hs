{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PassIntegration
  ( insertIntoPass
  , isPassAvailable
  , isPassInitialized
  , passEntryExists
  , PassError(..)
  , formatPassError
  ) where

import System.Process (proc, createProcess, CreateProcess(..)
                      , StdStream(..), waitForProcess, readCreateProcess)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import GHC.IO.Handle (hPutStr, hClose, hGetContents)
import Control.Exception (try, SomeException, IOException)
import Data.List (isInfixOf)

-- | Errors that can occur during pass integration
data PassError
  = PassNotInstalled
  | PassNotInitialized
  | PassEntryExists String   -- Entry path that already exists
  | PassInsertFailed String  -- Error message from pass
  | PassGPGError String      -- GPG-related errors
  | PassUnknownError String  -- Other errors
  deriving (Show, Eq)

-- | Check if pass is installed and available in PATH
isPassAvailable :: IO Bool
isPassAvailable = do
  result <- try $ readCreateProcess (proc "which" ["pass"]) ""
  case result of
    Left (_ :: IOException) -> return False
    Right output -> return (not (null output))

-- | Check if pass has been initialized (looks for ~/.password-store/.gpg-id)
isPassInitialized :: IO Bool
isPassInitialized = do
  result <- try getGpgIdPath
  case result of
    Left (_ :: IOException) -> return False
    Right path -> doesFileExist path
  where
    getGpgIdPath = do
      homeDir <- getHomeDirectory
      return $ homeDir </> ".password-store" </> ".gpg-id"

-- | Check if a pass entry already exists
passEntryExists :: String -> IO Bool
passEntryExists path = do
  homeDir <- getHomeDirectory
  let entryPath = homeDir </> ".password-store" </> path ++ ".gpg"
  doesFileExist entryPath

-- | Insert a passphrase into pass
insertIntoPass :: String  -- ^ Pass entry path (e.g., "github.com/username")
               -> String  -- ^ Passphrase to insert
               -> Bool    -- ^ Force overwrite
               -> IO (Either PassError ())
insertIntoPass path passphrase forceOverwrite = do
  -- Check if pass is available
  available <- isPassAvailable
  if not available
    then return $ Left PassNotInstalled
    else do
      -- Check if pass is initialized
      initialized <- isPassInitialized
      if not initialized
        then return $ Left PassNotInitialized
        else do
          -- Check if entry already exists
          exists <- passEntryExists path
          if exists && not forceOverwrite
            then return $ Left $ PassEntryExists path
            else doInsert
  where
    doInsert :: IO (Either PassError ())
    doInsert = do
      let args = ["insert", "--echo"]
                 ++ ["--force" | forceOverwrite]
                 ++ [path]
      result <- try $ createProcess (proc "pass" args)
                  { std_in = CreatePipe
                  , std_out = CreatePipe
                  , std_err = CreatePipe
                  }
      case result of
        Left (e :: SomeException) ->
          return $ Left $ PassUnknownError (show e)
        Right (Just hIn, Just _hOut, Just hErr, ph) -> do
          -- Write passphrase to stdin
          hPutStr hIn passphrase
          hClose hIn

          -- Wait for process to complete
          exitCode <- waitForProcess ph

          case exitCode of
            ExitSuccess -> return $ Right ()
            ExitFailure code -> do
              -- Read error output
              errMsg <- hGetContents hErr
              return $ Left $ classifyError code errMsg
        Right _ ->
          return $ Left $ PassUnknownError "Failed to create process handles"

    classifyError :: Int -> String -> PassError
    classifyError code errMsg
      | "gpg" `isInfixOf` errMsg = PassGPGError errMsg
      | otherwise = PassInsertFailed $ "Exit code " ++ show code ++ ": " ++ errMsg

-- | Format user-friendly error messages with setup hints
formatPassError :: PassError -> String
formatPassError PassNotInstalled = unlines
  [ "Error: pass is not installed or not in PATH"
  , ""
  , "To install pass:"
  , "  Arch Linux:    sudo pacman -S pass"
  , "  Ubuntu/Debian: sudo apt-get install pass"
  , "  macOS:         brew install pass"
  , ""
  , "Learn more: https://www.passwordstore.org/"
  ]

formatPassError PassNotInitialized = unlines
  [ "Error: pass has not been initialized"
  , ""
  , "To set up pass:"
  , "  1. Generate a GPG key (if you don't have one):"
  , "     gpg --full-generate-key"
  , ""
  , "  2. Find your GPG key ID:"
  , "     gpg --list-secret-keys --keyid-format LONG"
  , "     (Look for the ID after 'rsa4096/' or similar)"
  , ""
  , "  3. Initialize pass with your GPG key:"
  , "     pass init YOUR_GPG_ID"
  , ""
  , "Learn more: https://www.passwordstore.org/"
  ]

formatPassError (PassEntryExists path) = unlines
  [ "Error: An entry already exists for " ++ path
  , ""
  , "Use --pass-force to overwrite, or remove the existing entry:"
  , "  pass rm " ++ path
  ]

formatPassError (PassGPGError msg) = unlines
  [ "Error: GPG encryption failed"
  , ""
  , "Details: " ++ msg
  , ""
  , "Common solutions:"
  , "  1. Ensure GPG agent is running:"
  , "     export GPG_TTY=$(tty)"
  , ""
  , "  2. Verify your GPG key:"
  , "     gpg --list-secret-keys"
  , ""
  , "  3. Re-initialize pass if needed:"
  , "     pass init YOUR_GPG_ID"
  ]

formatPassError (PassInsertFailed msg) = unlines
  [ "Error: Failed to insert passphrase into pass"
  , ""
  , "Details: " ++ msg
  , ""
  , "Try running the pass command manually:"
  , "  pass insert path/to/entry"
  ]

formatPassError (PassUnknownError msg) = unlines
  [ "Error: An unexpected error occurred"
  , ""
  , "Details: " ++ msg
  ]
