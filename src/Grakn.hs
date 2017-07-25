module Grakn
  ( MatchQuery
  , Graph(Graph, keyspace, url)
  , GraknError
  , Result(MatchResult, InsertResult, AskResult, CountResult,
       DeleteResult)
  , Var
  , Name
  , Value(..)
  , defaultUrl
  , defaultKeyspace
  , execute
  , match
  , select
  , distinct
  , limit
  , var
  , name
  , isa
  , (-:)
  , (.:)
  , rp
  , (<:)
  , rel
  , has
  , hasText
  , var_
  ) where

import           Data.Text      (Text)
import           Grakn.Client   (GraknError, Graph (Graph, keyspace, url), Result (AskResult, CountResult, DeleteResult, InsertResult, MatchResult),
                                 defaultKeyspace, defaultUrl, execute)
import           Grakn.Pattern
import           Grakn.Property
import           Grakn.Query
import           Grakn.Util     (Convert)

-- |Specify a property has a particular type
(-:) :: (Convert p Pattern, Convert a VarOrName) => p -> a -> Pattern
(-:) = isa

-- |Shorthand to define a relation
rel :: Convert a RolePlayer => [a] -> Pattern
rel = (var_ <:)

-- |Specify a property has a resource
hasText :: (Convert p Pattern) => p -> Name -> Text -> Pattern
hasText = has
