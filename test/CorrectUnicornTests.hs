module CorrectUnicornTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import CorrectUnicorn

tests :: TestTree
tests = testGroup "CorrectUnicorn tests"
  [ testCase "Settings word count" $
      settingsWordCount (Settings 5 Nothing Nothing False Nothing False False False) @?= 5

  , testGroup "Security calculations"
      [ testCase "calculateEntropy with dict=100, words=4" $
          let entropy = calculateEntropy 100 4
          in abs (entropy - 26.575) < 0.01 @? "Expected ~26.6 bits, got " ++ show entropy

      , testCase "calculateEntropy with dict=2163, words=4" $
          let entropy = calculateEntropy 2163 4
          in abs (entropy - 44.325) < 0.01 @? "Expected ~44.3 bits, got " ++ show entropy

      , testCase "calculateKeyspace with dict=100, words=4" $
          calculateKeyspace 100 4 @?= 100000000

      , testCase "calculateKeyspace with dict=2163, words=4" $
          calculateKeyspace 2163 4 @?= 21889007887761

      , testCase "estimateCrackTime with keyspace=1000000, rate=1000/sec" $
          estimateCrackTime 1000000 1000 @?= 1000.0

      , testCase "estimateCrackTime with keyspace=1000000, rate=1000000/sec" $
          estimateCrackTime 1000000 1000000 @?= 1.0
      ]
  ]
