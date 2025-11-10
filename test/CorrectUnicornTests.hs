module CorrectUnicornTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import CorrectUnicorn

tests :: TestTree
tests = testGroup "CorrectUnicorn tests"
  [ testCase "Settings word count" $
      settingsWordCount (Settings 5 Nothing Nothing False Nothing) @?= 5
  ]
