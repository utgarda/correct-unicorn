module CorrectUnicornTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import CorrectUnicorn
import PrettyAnsi (paintWords)
import Data.Char (isUpper)
import Prelude hiding (words)

tests :: TestTree
tests = testGroup "CorrectUnicorn tests"
  [ testCase "Settings word count" $
      settingsWordCount (Settings 5 Nothing Nothing False Nothing False False False) @?= 5

  , testGroup "Security calculations (unit tests)"
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

  , testGroup "Property tests"
      [ testGroup "Helper functions"
          [ testProperty "stripAnsi is idempotent" prop_stripAnsiIdempotent
          , testProperty "stripAnsi preserves non-ANSI text" prop_stripAnsiPreservesText
          , testProperty "capitalizeWords preserves word count" prop_capitalizePreservesCount
          , testProperty "capitalizeWords only changes first character" prop_capitalizeFirstOnly
          , testProperty "joinWithSeparator creates correct number of separators" prop_joinSeparatorCount
          , testProperty "paintWords preserves word count" prop_paintWordsPreservesCount
          , testProperty "resolveColors returns defaults when empty" prop_resolveColorsDefaults
          ]

      , testGroup "Security calculations"
          [ testProperty "entropy increases with more words" prop_entropyIncreasesWithWords
          , testProperty "entropy increases with larger dictionary" prop_entropyIncreasesWithDict
          , testProperty "keyspace calculation is correct" prop_keyspaceCalculation
          , testProperty "crack time is inversely proportional to rate" prop_crackTimeInverse
          ]
      ]
  ]

-- Helper function properties

prop_stripAnsiIdempotent :: Property
prop_stripAnsiIdempotent = property $ do
  text <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
  stripAnsi (stripAnsi text) === stripAnsi text

prop_stripAnsiPreservesText :: Property
prop_stripAnsiPreservesText = property $ do
  text <- forAll $ Gen.string (Range.linear 0 50) Gen.alpha
  stripAnsi text === text

prop_capitalizePreservesCount :: Property
prop_capitalizePreservesCount = property $ do
  words <- forAll $ Gen.list (Range.linear 0 10) (Gen.string (Range.linear 1 10) Gen.alpha)
  length (capitalizeWords words) === length words

prop_capitalizeFirstOnly :: Property
prop_capitalizeFirstOnly = property $ do
  word <- forAll $ Gen.string (Range.linear 1 10) Gen.lower
  case capitalizeWords [word] of
    [capitalized] -> do
      case (word, capitalized) of
        ([], []) -> success
        (_:cs, cap:caps) -> do
          isUpper cap === True
          caps === cs
        _ -> failure
    _ -> failure

prop_joinSeparatorCount :: Property
prop_joinSeparatorCount = property $ do
  n <- forAll $ Gen.int (Range.linear 0 10)
  sep <- forAll $ Gen.constant "-"  -- Use a separator that won't appear in words
  words <- forAll $ Gen.list (Range.singleton n) (Gen.constant "abc")  -- Use words without '-'
  let result = joinWithSeparator sep words
  let sepCount = length $ filter (== '-') result
  if n <= 1
    then sepCount === 0
    else sepCount === (n - 1)  -- Exact count of separators

prop_paintWordsPreservesCount :: Property
prop_paintWordsPreservesCount = property $ do
  words <- forAll $ Gen.list (Range.linear 0 10) (Gen.string (Range.linear 1 10) Gen.alpha)
  colors <- forAll $ Gen.list (Range.linear 1 5) (Gen.string (Range.linear 1 10) Gen.alpha)
  length (paintWords words colors) === length words

prop_resolveColorsDefaults :: Property
prop_resolveColorsDefaults = property $ do
  let result = resolveColors []
  length result === 2  -- Should return default blue and yellow

-- Security calculation properties

prop_entropyIncreasesWithWords :: Property
prop_entropyIncreasesWithWords = property $ do
  dictSize <- forAll $ Gen.int (Range.linear 10 1000)
  words1 <- forAll $ Gen.int (Range.linear 1 10)
  words2 <- forAll $ Gen.int (Range.linear 1 10)
  let entropy1 = calculateEntropy dictSize words1
  let entropy2 = calculateEntropy dictSize words2
  if words1 < words2
    then assert (entropy1 < entropy2)
    else if words1 > words2
      then assert (entropy1 > entropy2)
      else entropy1 === entropy2

prop_entropyIncreasesWithDict :: Property
prop_entropyIncreasesWithDict = property $ do
  dict1 <- forAll $ Gen.int (Range.linear 10 1000)
  dict2 <- forAll $ Gen.int (Range.linear 10 1000)
  wordCount <- forAll $ Gen.int (Range.linear 1 10)
  let entropy1 = calculateEntropy dict1 wordCount
  let entropy2 = calculateEntropy dict2 wordCount
  if dict1 < dict2
    then assert (entropy1 < entropy2)
    else if dict1 > dict2
      then assert (entropy1 > entropy2)
      else entropy1 === entropy2

prop_keyspaceCalculation :: Property
prop_keyspaceCalculation = property $ do
  dictSize <- forAll $ Gen.int (Range.linear 2 100)
  wordCount <- forAll $ Gen.int (Range.linear 1 5)
  let keyspace = calculateKeyspace dictSize wordCount
  let expected = toInteger dictSize ^ wordCount
  keyspace === expected

prop_crackTimeInverse :: Property
prop_crackTimeInverse = property $ do
  keyspace <- forAll $ Gen.integral (Range.linear 1000 1000000)
  rate <- forAll $ Gen.double (Range.linearFrac 1.0 10000.0)
  let time1 = estimateCrackTime keyspace rate
  let time2 = estimateCrackTime keyspace (rate * 2)
  -- time2 should be approximately half of time1
  assert (abs (time2 * 2 - time1) < time1 * 0.01)  -- Within 1% tolerance
