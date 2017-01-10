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
import           Data.Text        (Text)

-- |Specify a property has a particular type
(-:) :: (IsPattern p, IsVarOrName a) => p -> a -> Pattern
(-:) = isa

-- |Shorthand to define a relation
rel :: IsRolePlayer a => [a] -> Pattern
rel = (var_ <:)

-- |Specify a property has a resource
hasText :: (IsPattern p) => p -> Name -> Text -> Pattern
hasText = has
