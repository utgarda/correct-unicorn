module CorrectUnicornTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import CorrectUnicorn

tests :: TestTree
tests = testGroup "CorrectUnicorn tests"
  [ testCase "Settings count" $
      count (Settings 5) @?= 5
  ]
