module ClientSpec
  ( spec
  ) where

import           Data.Either    (isLeft)
import           Grakn
import           Servant.Client
import           Test.Hspec

spec :: Spec
spec =
  describe
    "As a Grakn Developer I should be able to connect to a running Grakn Instance and use that instance to issue queries." $
  it "Issuing a query with a broken connection" $ do
    let graph = aBrokenConnectionToTheDatabase
    result <- execute graph "match $x sub entitty"
    result `shouldSatisfy` isLeft

aBrokenConnectionToTheDatabase :: Graph
aBrokenConnectionToTheDatabase =
  Graph (BaseUrl Http "1.2.3.4" 5678 "") "akeyspace"
