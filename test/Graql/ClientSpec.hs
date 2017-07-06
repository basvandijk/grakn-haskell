module Graql.ClientSpec where

import           Graql.Client
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  it "The graph's keyspace is set correctly" $
    property $ \u k ->
      let graph = Graph {uri = u, keyspace = k}
      in keyspace graph `shouldBe` k
  it "The graph's URI is set correctly" $
    property $ \u k ->
      let graph = Graph {uri = u, keyspace = k}
      in uri graph `shouldBe` u
