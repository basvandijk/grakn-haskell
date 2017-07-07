{-# LANGUAGE OverloadedStrings #-}

module Graql.Client
  ( Graph(Graph, keyspace, url)
  , Concept(Concept, cid, cname, ctype, value)
  , GraknError
  , Result(MatchResult, AskResult, CountResult)
  , defaultUrl
  , defaultKeyspace
  , execute
  ) where

import           Control.Applicative      (empty)
import           Control.Monad.Except     (runExceptT)
import           Data.Aeson               (FromJSON, ToJSON, encode, parseJSON,
                                           (.:), (.:?))
import qualified Data.Aeson               as Aeson
import           Data.Foldable            (asum)
import           Data.Map                 (Map)
import           Data.Proxy               (Proxy (Proxy))
import           Data.Text                (Text)
import           Graql.Property           (Name, Value, Var)
import           Graql.Query              (IsQuery (queryString))
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)
import           Network.HTTP.Media       ((//))
import           Servant.API
import           Servant.API.ContentTypes (eitherDecodeLenient)
import           Servant.Client

data Graph = Graph
  { url      :: BaseUrl
  , keyspace :: String
  }

newtype GraknError =
  GraknError String
  deriving (Eq, Show)

-- |A result of a match query, binding variables to concepts
data Result
  = MatchResult [Map Var Concept]
  | AskResult Bool
  | CountResult Integer
  deriving (Show, Eq)

-- |A concept in the graph
data Concept = Concept
  { cid   :: Text
  , cname :: Maybe Name
  , ctype :: Maybe Name
  , value :: Maybe Value
  } deriving (Show, Eq)

-- |The default Grakn URL, accessing localhost
defaultUrl :: BaseUrl
defaultUrl = BaseUrl Http "localhost" 4567 ""

-- |The default Grakn keyspace
defaultKeyspace :: String
defaultKeyspace = "grakn"

execute :: IsQuery q => Graph -> q -> IO (Either ServantError Result)
execute graph query = do
  manager <- newManager defaultManagerSettings
  runExceptT $ graqlGet graph (queryString query) manager

-- |A type describing the REST API we use to execute queries
type GraknAPI
   = "graph" :> "graql" :> QueryParam "query" String :> QueryParam "keyspace" String :> QueryParam "infer" Bool :> QueryParam "materialise" Bool :> Get '[ GraqlJSON] Result

graknAPI :: Proxy GraknAPI
graknAPI = Proxy

graqlGet :: Graph -> String -> Manager -> ClientM Result
graqlGet (Graph u ks) query manager =
  client graknAPI (Just query) (Just ks) (Just False) (Just False) manager u

instance FromJSON Concept where
  parseJSON (Aeson.Object obj) =
    Concept <$> (obj .: "id") <*> (obj .:? "name") <*> (obj .:? "isa") <*>
    (obj .:? "value")
  parseJSON _ = empty

instance FromJSON Result where
  parseJSON (Aeson.Object obj) =
    asum
      [ MatchResult <$> (obj .: "response")
      , AskResult <$> (obj .: "response")
      , CountResult <$> (obj .: "response")
      ]
  parseJSON _ = empty

data GraqlJSON

instance Accept GraqlJSON where
  contentType _ = "application" // "graql+json"

instance ToJSON a => MimeRender GraqlJSON a where
  mimeRender _ = encode

instance FromJSON a => MimeUnrender GraqlJSON a where
  mimeUnrender _ = eitherDecodeLenient
