module Graql
    ( MatchQuery
    , Var
    , Name
    , Value (..)
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
    
import           Graql.Property
import           Graql.Pattern
import           Graql.Query
import           Graql.Util       (Convert)
import           Data.Text        (Text)

-- |Specify a property has a particular type
(-:) :: (Convert p Pattern, Convert a VarOrName) => p -> a -> Pattern
(-:) = isa

-- |Shorthand to define a relation
rel :: Convert a RolePlayer => [a] -> Pattern
rel = (var_ <:)

-- |Specify a property has a resource
hasText :: (Convert p Pattern) => p -> Name -> Text -> Pattern
hasText = has
