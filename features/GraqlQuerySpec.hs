module GraqlQuerySpec (spec) where

import           Env
import           Graql
import           Test.Hspec

aGraphContainingTypesAndInstances :: IO Graph
aGraphContainingTypesAndInstances = do
  graph <- givenAGraph
  envInsert "person sub entity, has name; name sub resource, datatype string;"
  envInsert "$alice isa person, has name \"Alice\";"
  return graph

givenAGraph :: IO Graph
givenAGraph = Graph defaultUrl <$> envKeyspace

spec :: Spec
spec =
  before aGraphContainingTypesAndInstances $
  describe
    "As a Grakn Developer, I should be able to interact with a Grakn Graph using Graql queries" $ do
    it "Match Query With Empty Response" $ \graph -> do
      response <- execute graph "match $x isa person, has name \"Precy\";"
      response `hasResults` 0
    it "Match Query With Non-Empty Response" $ \graph -> do
      response <- execute graph "match $x isa person, has name \"Alice\";"
      response `hasResults` 1
    it "Ask Query With False Response" $ \graph -> do
      response <- execute graph "match $x has name \"Precy\"; ask;"
      response `is` AskResult False
    it "Ask Query With True Response" $ \graph -> do
      response <- execute graph "match $x has name \"Alice\"; ask;"
      response `is` AskResult True
    it "Aggregate Query" $ \graph -> do
      response <- execute graph "match $x isa person; aggregate count;"
      response `is` CountResult 1
    it "Compute Query" $ \graph -> do
      response <- execute graph "compute count in person;"
      response `is` CountResult 1

is :: (Show t, Show a, Eq a) => Either t a -> a -> Expectation
response `is` expected =
  case response of
    Right answer -> answer `shouldBe` expected
    x            -> expectationFailure (show x)

hasResults :: Show t => Either t Result -> Int -> Expectation
response `hasResults` n =
  case response of
    Right (MatchResult answers) -> length answers `shouldBe` n
    x                           -> expectationFailure (show x)
