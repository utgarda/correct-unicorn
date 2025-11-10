module Main where

import Test.Tasty

import qualified PrettyAnsiTests
import qualified CorrectUnicornTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "correct-unicorn tests"
  [ PrettyAnsiTests.tests
  , CorrectUnicornTests.tests
  ]
