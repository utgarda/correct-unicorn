module PrettyAnsiTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (isPrefixOf)

import PrettyAnsi

tests :: TestTree
tests = testGroup "PrettyAnsi tests"
  [ testCase "paintWords with single color" $
      paintWords ["hello"] [ansiGreen] @?= [ansiGreen ++ "hello"]

  , testCase "paintWords with two colors, two words" $
      paintWords ["hello", "world"] [ansiGreen, ansiYellow] @?=
        [ansiGreen ++ "hello", ansiYellow ++ "world"]

  , testCase "paintWords cycles colors when more words than colors" $
      paintWords ["one", "two", "three", "four"] [ansiGreen, ansiYellow] @?=
        [ansiGreen ++ "one", ansiYellow ++ "two", ansiGreen ++ "three", ansiYellow ++ "four"]

  , testCase "paintWords with empty word list" $
      paintWords [] [ansiGreen] @?= []

  , testCase "bold prepends bold code" $
      bold "text" @?= "\x1b[1m" ++ "text"

  , testCase "ansiBold is correct ANSI code" $
      ansiBold @?= "\x1b[1m"
  ]
