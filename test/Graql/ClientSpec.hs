module Graql.ClientSpec where

import Test.Hspec
import Test.QuickCheck
import Graql.Client

spec :: Spec
spec = do
    it "The graph's keyspace is set correctly" $ property $ \u k ->
        let graph = Graph {uri=u, keyspace=k} in
        keyspace graph `shouldBe` k

    it "The graph's URI is set correctly" $ property $ \u k ->
        let graph = Graph {uri=u, keyspace=k} in
        uri graph `shouldBe` u
