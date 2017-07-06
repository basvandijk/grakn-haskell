module Steps.GraqlQuerySpec where

import Test.Hspec
import Graql
import Control.Monad (when)
import System.IO (hGetContents)
import System.Process (StdStream (CreatePipe), readProcess, readProcessWithExitCode, proc, std_out)
import Data.String.Utils (rstrip)

aGraphContainingTypesAndInstances :: IO Graph
aGraphContainingTypesAndInstances = do
    -- Given a graph
    keyspace <- rstrip <$> readProcess "grakn-spec/env.sh" ["keyspace"] ""
    let graph = Graph "http://localhost:4567" keyspace
    -- And ontology `person sub entity, has name; name sub resource, datatype string;`
    readProcess "grakn-spec/env.sh" ["insert", "person sub entity, has name; name sub resource, datatype string;"] ""
    -- And data `$alice isa person, has name "Alice";`
    readProcess "grakn-spec/env.sh" ["insert", "$alice isa person, has name \"Alice\";"] ""
    return graph

spec :: Spec
spec = before aGraphContainingTypesAndInstances $
    describe "As a Grakn Developer, I should be able to interact with a Grakn Graph using Graql queries" $ do

--    Scenario: Valid Insert Query for Types
--        When the user issues `insert $x label dog sub entity;`
--        Then the type "dog" is in the graph
--        And return a response with new concepts
--
--    Scenario: Redundant Insert Query
--        When the user issues `insert $x label person sub entity;`
--        Then return a response with existing concepts
--
--    Scenario: Valid Insert Query for Instances
--        When the user issues `insert $bob isa person, has name "Bob";`
--        Then the instance with name "Bob" is in the graph
--        And return a response with new concepts
--
--    Scenario: Invalid Insert Query
--        When the user issues `insert $dunstan isa dog, has name "Dunstan";`
--        Then return an error

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

--    Scenario: Successful Delete Query
--        Given ontology `dog sub entity;`
--        When the user issues `match $x label dog; delete $x;`
--        Then the response is empty
--
--    Scenario: Unsuccessful Delete Query
--        When the user issues `match $x label person; delete $x;`
--        Then return an error
--
--    Scenario: Delete Query for non Existent Concept
--        When the user issues `match $x has name "Precy"; delete $x;`
--        Then the response is empty

response `is` expected =
    case response of
        Right answer -> answer `shouldBe` expected
        x            -> expectationFailure (show x)

response `hasResults` n =
    case response of
        Right (MatchResult answers) -> length answers `shouldBe` n
        x                           -> expectationFailure (show x)
