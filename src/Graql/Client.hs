{-# LANGUAGE OverloadedStrings #-}

module Graql.Client
  ( Graph(Graph, keyspace, uri)
  , GraknError
  , Result (MatchResult, AskResult, CountResult)
  , execute
  ) where

import           Control.Applicative (empty)
import Graql.Query (IsQuery (queryString))
import Graql.Property (Var, Name, Value)
import Control.Monad.Except (runExceptT)
import Data.Foldable (asum)
import           Data.Map           (Map)
import           Data.Text          (Text)
import qualified Data.Aeson         as Aeson
import           Data.Aeson         (FromJSON, ToJSON, encode, parseJSON,
                                     (.:), (.:?))
import Servant.API
import Servant.API.ContentTypes (eitherDecodeLenient)
import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Media ((//))
import Data.Proxy (Proxy (Proxy))

data Graph = Graph { uri :: String, keyspace :: String }

newtype GraknError = GraknError String deriving (Eq, Show)


-- |A result of a match query, binding variables to concepts
data Result = MatchResult [Map Var Concept]
            | AskResult Bool
            | CountResult Integer
            deriving (Show, Eq)

-- |A concept in the graph
data Concept = Concept { cid :: Text, cname :: Maybe Name, ctype :: Maybe Name, value :: Maybe Value }
  deriving (Show, Eq)

execute :: IsQuery q => Graph -> q -> IO (Either ServantError Result)
execute (Graph uri keyspace) query = do
    manager <- newManager defaultManagerSettings
    runExceptT $ graqlGet' (queryString query) keyspace manager

graqlGet' :: String -> String -> Manager -> ClientM Result
graqlGet' query keyspace manager = graqlGet (Just query) (Just keyspace) (Just False) (Just False) manager (BaseUrl Http "localhost" 4567 "")

instance FromJSON Concept where
  parseJSON (Aeson.Object obj) =
    Concept <$> (obj .: "id") <*> (obj .:? "name") <*> (obj .:? "isa") <*> (obj .:? "value")
  parseJSON _ = empty

instance FromJSON Result where
  parseJSON (Aeson.Object obj) = asum [ MatchResult <$> (obj .: "response")
                                      , AskResult <$> (obj .: "response")
                                      , CountResult <$> (obj .: "response")
                                      ]
  parseJSON _ = empty


type GraknAPI =
       "graph" :> "graql" :>
       QueryParam "query" String :>
       QueryParam "keyspace" String :>
       QueryParam "infer" Bool :>
       QueryParam "materialise" Bool :>
       Get '[GraqlJSON] Result

graknAPI :: Proxy GraknAPI
graknAPI = Proxy

graqlGet :: Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Manager -> BaseUrl -> ClientM Result
graqlGet = client graknAPI


data GraqlJSON

instance Accept GraqlJSON where
    contentType _ = "application" // "graql+json"

instance ToJSON a => MimeRender GraqlJSON a where
    mimeRender _ = encode

instance FromJSON a => MimeUnrender GraqlJSON a where
    mimeUnrender _ = eitherDecodeLenient