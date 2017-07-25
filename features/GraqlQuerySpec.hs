module GraqlQuerySpec (spec) where

import           Control.Exception (Exception, displayException)
import           Data.Either       (isLeft)
import           Env
import           Grakn
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

    it "Valid Insert Query for Types" $ \graph -> do
      response <- execute graph "insert $x label dog sub entity;"
      typeIsInTheGraph "dog"
      response `hasResults` 1

    it "Redundant Insert Query" $ \graph -> do
      response <- execute graph "insert $x label person sub entity;"
      response `hasResults` 1

    it "Valid Insert Query for Instances" $ \graph -> do
      response <- execute graph "insert $bob isa person, has name \"Bob\";"
      instanceIsInTheGraph "name" "Bob"
      response `hasResults` 1

    it "Invalid Insert Query" $ \graph -> do
      response <- execute graph "insert $dunstan isa dog, has name \"Dunstan\";"
      isAnError response

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

    it "Successful Delete Query" $ \graph -> do
      envInsert "dog sub entity;"
      response <- execute graph "match $x label dog; delete $x;"
      response `is` DeleteResult

    it "Unsuccessful Delete Query" $ \graph -> do
      response <- execute graph "match $x label person; delete $x;"
      isAnError response

    it "Delete Query for non Existent Concept" $ \graph -> do
      response <- execute graph "match $x has name \"Precy\"; delete $x;"
      response `is` DeleteResult

is :: (Exception e, Show a, Eq a) => Either e a -> a -> Expectation
response `is` expected =
  case response of
    Right answer -> answer `shouldBe` expected
    Left  err    -> expectationFailure (displayException err)

isAnError :: Either t a -> Expectation
isAnError response = isLeft response `shouldBe` True

typeIsInTheGraph :: String -> Expectation
typeIsInTheGraph label = do
    isIn <- envCheckType label
    isIn `shouldBe` True

instanceIsInTheGraph :: String -> String -> Expectation
instanceIsInTheGraph res val = do
    isIn <- envCheckInstance res val
    isIn `shouldBe` True

hasResults :: Exception e => Either e Result -> Int -> Expectation
response `hasResults` n =
  case response of
    Right (MatchResult  answers) -> length answers `shouldBe` n
    Right (InsertResult answers) -> length answers `shouldBe` n
    Left  err                    -> expectationFailure (displayException err)
    x                            -> expectationFailure (show x)
