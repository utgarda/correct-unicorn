module CorrectUnicornTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import CorrectUnicorn
import Config (RuntimeConfig(..))
import PrettyAnsi (paintWords)
import Data.Char (isUpper)
import System.Random (mkStdGen)
import qualified Data.Text as T
import qualified Data.Map as Map
import Prelude hiding (words)
import qualified Prelude as P

tests :: TestTree
tests = testGroup "CorrectUnicorn tests"
  [ testCase "Settings word count" $
      settingsWordCount (Settings 5 Nothing Nothing False Nothing False False False False Nothing False) @?= 5

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

  , testGroup "Word filtering (unit tests)"
      [ testCase "filterWords removes words with apostrophe" $
          filterWords "'" ["hello", "don't", "world", "it's"] @?= ["hello", "world"]

      , testCase "filterWords with empty stop list returns all words" $
          filterWords "" ["hello", "don't", "world"] @?= ["hello", "don't", "world"]

      , testCase "filterWords with multiple stop chars" $
          filterWords "'-" ["hello", "don't", "world", "self-aware"] @?= ["hello", "world"]

      , testCase "defaultStopChars contains apostrophe" $
          defaultStopChars @?= "'"

      , testCase "filterWords preserves order" $
          filterWords "'" ["apple", "can't", "banana", "won't", "cherry"] @?= ["apple", "banana", "cherry"]

      , testCase "filterWords handles empty input" $
          filterWords "'" [] @?= []

      , testCase "filterWords handles all words filtered" $
          filterWords "'" ["don't", "won't", "can't", "it's"] @?= []

      , testCase "filterWords with possessives and contractions" $
          filterWords "'" ["Aaron's", "aardvark's", "don't", "won't", "hello", "world"] @?= ["hello", "world"]

      , testCase "filterWords filters both lowercase and uppercase" $
          filterWords "aA" ["apple", "banana", "Apricot", "grape"] @?= []

      , testCase "filterWords is case-sensitive" $
          filterWords "a" ["Apple", "Banana", "Grape"] @?= ["Apple"]
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
          , testProperty "filterWords removes all words with stop chars" prop_filterWordsRemovesStopChars
          ]

      , testGroup "Security calculations"
          [ testProperty "entropy increases with more words" prop_entropyIncreasesWithWords
          , testProperty "entropy increases with larger dictionary" prop_entropyIncreasesWithDict
          , testProperty "keyspace calculation is correct" prop_keyspaceCalculation
          , testProperty "crack time is inversely proportional to rate" prop_crackTimeInverse
          ]

      , testGroup "Word filtering properties"
          [ testProperty "filterWords is idempotent" prop_filterWordsIdempotent
          , testProperty "filtered list is subset of original" prop_filterWordsSubset
          , testProperty "filterWords with defaultStopChars removes apostrophes" prop_filterDefaultRemovesApostrophes
          , testProperty "generated passphrases contain no stop chars" prop_generatedNoStopChars
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

prop_filterWordsRemovesStopChars :: Property
prop_filterWordsRemovesStopChars = property $ do
  stopChars <- forAll $ Gen.string (Range.linear 1 5) Gen.alpha
  words <- forAll $ Gen.list (Range.linear 0 20) (Gen.string (Range.linear 1 10) Gen.alpha)
  let filtered = filterWords stopChars words
  -- No word in filtered list should contain any stop character
  assert (all (\word -> not (any (`elem` word) stopChars)) filtered)

-- Word filtering property tests

prop_filterWordsIdempotent :: Property
prop_filterWordsIdempotent = property $ do
  stopChars <- forAll $ Gen.string (Range.linear 0 5) Gen.alpha
  words <- forAll $ Gen.list (Range.linear 0 20) (Gen.string (Range.linear 1 10) Gen.alpha)
  let filtered1 = filterWords stopChars words
  let filtered2 = filterWords stopChars filtered1
  filtered1 === filtered2

prop_filterWordsSubset :: Property
prop_filterWordsSubset = property $ do
  stopChars <- forAll $ Gen.string (Range.linear 0 5) Gen.alpha
  words <- forAll $ Gen.list (Range.linear 0 20) (Gen.string (Range.linear 1 10) Gen.alpha)
  let filtered = filterWords stopChars words
  -- Every word in filtered must exist in original
  assert (all (`elem` words) filtered)
  -- Filtered list cannot be longer than original
  assert (length filtered <= length words)

prop_filterDefaultRemovesApostrophes :: Property
prop_filterDefaultRemovesApostrophes = property $ do
  -- Generate words with and without apostrophes
  cleanWords <- forAll $ Gen.list (Range.linear 0 10) (Gen.string (Range.linear 1 10) Gen.alpha)
  let wordsWithApostrophes = ["don't", "won't", "can't", "it's", "I'm"]
  let allWords = cleanWords ++ wordsWithApostrophes
  let filtered = filterWords defaultStopChars allWords
  -- No word in filtered should contain an apostrophe
  assert (all (\word -> '\'' `notElem` word) filtered)

prop_generatedNoStopChars :: Property
prop_generatedNoStopChars = property $ do
  -- Generate a dictionary with some words containing apostrophes
  cleanWords <- forAll $ Gen.list (Range.linear 10 50) (Gen.string (Range.linear 3 10) Gen.alpha)
  let wordsWithApostrophes = ["don't", "won't", "can't", "it's"]
  let dictionary = cleanWords ++ wordsWithApostrophes
  let filteredDict = filterWords defaultStopChars dictionary

  -- Skip test if filtered dict is empty
  if null filteredDict
    then discard
    else do
      wordCount <- forAll $ Gen.int (Range.linear 1 5)
      gen <- forAll $ Gen.element [42..100]  -- Use predictable seed range

      -- Create a minimal RuntimeConfig for testing
      let testConfig = RuntimeConfig
            { runtimeDictPath = T.pack ""  -- Not used in generateWithWordCount
            , runtimeWordCount = wordCount
            , runtimeSeparator = T.pack " "
            , runtimeColors = []
            , runtimeNoColor = True
            , runtimeBold = False
            , runtimeSubstitutions = Map.empty
            , runtimeMinWords = 0
            , runtimeMinWordLength = 0
            , runtimeMaxWordLength = 100
            , runtimeMinChars = Nothing
            , runtimeCapitalize = False
            }

      let stdGen = mkStdGen gen
      let output = generateWithWordCount testConfig filteredDict wordCount stdGen
      let outputWords = P.words output  -- Split by whitespace

      -- Verify no word contains an apostrophe
      assert (all (\word -> '\'' `notElem` word) outputWords)
