module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import CorrectUnicorn

main :: IO ()
main = genPassword =<< execParser opts
  where
    opts = info (settings <**> helper)
      ( fullDesc
     <> progDesc "passphrase generator inspired by xkcd 936"
     <> header "have fun!" )
