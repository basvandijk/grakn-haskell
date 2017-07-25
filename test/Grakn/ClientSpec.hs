module Grakn.ClientSpec (spec) where

import           Grakn.Client
import qualified Servant.Client  as S
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  it "The graph's keyspace is set correctly" $
    property $ \(BaseUrl u) k ->
      let graph = Graph {url = u, keyspace = k}
      in keyspace graph `shouldBe` k
  it "The graph's URI is set correctly" $
    property $ \(BaseUrl u) k ->
      let graph = Graph {url = u, keyspace = k}
      in url graph `shouldBe` u

newtype BaseUrl =
  BaseUrl S.BaseUrl
  deriving (Show)

instance Arbitrary BaseUrl where
  arbitrary = do
    let scheme = elements [S.Http, S.Https]
    BaseUrl <$> (S.BaseUrl <$> scheme <*> arbitrary <*> arbitrary <*> arbitrary)
