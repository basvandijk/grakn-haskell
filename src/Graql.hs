module Graql
    ( MatchQuery
    , Graph (Graph, keyspace, uri)
    , GraknError
    , Result (MatchResult, AskResult, CountResult)
    , Var
    , Name
    , Value (..)
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
import           Graql.Client   (GraknError, Graph (Graph, keyspace, uri),
                                 Result (MatchResult, AskResult, CountResult),
                                 execute)
import           Graql.Pattern
import           Graql.Property
import           Graql.Query
import           Graql.Util     (Convert)

-- |Specify a property has a particular type
(-:) :: (Convert p Pattern, Convert a VarOrName) => p -> a -> Pattern
(-:) = isa

-- |Shorthand to define a relation
rel :: Convert a RolePlayer => [a] -> Pattern
rel = (var_ <:)

-- |Specify a property has a resource
hasText :: (Convert p Pattern) => p -> Name -> Text -> Pattern
hasText = has
