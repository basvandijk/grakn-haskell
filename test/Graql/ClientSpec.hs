module Graql.ClientSpec where

import           Graql.Client
import           Test.Hspec
import           Test.QuickCheck
import           Servant.API
import           Servant.Client

spec :: Spec
spec = do
  it "The graph's keyspace is set correctly" $
    property $ \u k ->
      let graph = Graph {url = u, keyspace = k}
      in keyspace graph `shouldBe` k
  it "The graph's URI is set correctly" $
    property $ \u k ->
      let graph = Graph {url = u, keyspace = k}
      in url graph `shouldBe` u


instance Arbitrary BaseUrl where
    arbitrary = BaseUrl <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Scheme where
    arbitrary = elements [Http, Https]