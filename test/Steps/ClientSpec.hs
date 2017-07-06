module Steps.ClientSpec where

import           Data.Either (isLeft)
import           Graql
import           Test.Hspec

spec :: Spec
spec = describe "As a Grakn Developer I should be able to connect to a running Grakn Instance and use that instance to issue queries." $

    it "Issuing a query with a broken connection" $ do
        -- Given a broken connection to the database
        let graph = aBrokenConnectionToTheDatabase
        -- When the user issues `match $x sub entity;`
        result <- execute graph "match $x sub entitty"
        -- Then return an error
        result `shouldSatisfy` isLeft
--
--    @skip
--    Scenario: Creating a connection to a graph
--        Given a graph which exists
--        When the user connects to the graph
--        Then return a usable connection
--
--    @skip
--    Scenario: Creating a connection to a non-existant graph
--        Given a graph which does not exist
--        When the user connects to the graph
--        Then create a new graph


aBrokenConnectionToTheDatabase :: Graph
aBrokenConnectionToTheDatabase = Graph "http://1.2.3.4:5678" "akeyspace"
