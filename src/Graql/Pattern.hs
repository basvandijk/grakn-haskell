module Graql.Pattern
    ( Pattern
    , IsPattern
    , var_
    , isa
    , (<:)
    , has
    ) where

import           Graql.Util
import           Graql.Property

-- |A pattern to find in the graph
data Pattern = Pattern (Maybe VarOrId) [Property]

-- |Represents things that can be patterns
class IsPattern a where
    toPattern :: a -> Pattern

-- |Create an anonymous variable
var_ :: Pattern
var_ = Pattern Nothing []

-- |Specify a property has a particular type
isa :: (IsPattern p, IsVarOrId a) => p -> a -> Pattern
patt `isa` x = addProperty patt (Isa $ toVarOrId x)

-- |Specify a property is a relation between other variables
(<:) :: (IsPattern p, IsCasting a) => p -> [a] -> Pattern
patt <: cs = addProperty patt (Rel $ map toCasting cs)

-- |Specify a property has a resource
has :: (IsPattern p, IsResource a) => p -> Id -> a -> Pattern
has patt rt v = addProperty patt (Has rt $ toResource v)


showProps :: Show a => [a] -> String
showProps = spaces . reverse

addPropToPattern :: Pattern -> Property -> Pattern
addPropToPattern (Pattern name props) prop = Pattern name (prop : props)

addProperty :: IsPattern a => a -> Property -> Pattern
addProperty = addPropToPattern . toPattern


instance Show Pattern where
    show (Pattern (Just v) props) = show v ++ " " ++ showProps props ++ ";"
    show (Pattern Nothing  props) = showProps props ++ ";"

instance IsPattern Pattern where
    toPattern = id

instance IsPattern Var where
    toPattern = toPattern . toVarOrId

instance IsPattern Id where
    toPattern = toPattern . toVarOrId

instance IsPattern VarOrId where
    toPattern v = Pattern (Just v) []
