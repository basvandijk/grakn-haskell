module Graql
    ( MatchQuery
    , Var
    , Id
    , Value (..)
    , match
    , select
    , distinct
    , limit
    , var
    , gid
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
import           Data.Text        (Text)

-- |Specify a property has a particular type
(-:) :: (IsPattern p, IsVarOrId a) => p -> a -> Pattern
(-:) = isa

-- |Shorthand to define a relation
rel :: IsCasting a => [a] -> Pattern
rel = (var_ <:)

-- |Specify a property has a resource
hasText :: (IsPattern p) => p -> Id -> Text -> Pattern
hasText = has
