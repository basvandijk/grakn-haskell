module Graql.Pattern
  ( Pattern
  , var_
  , isa
  , (<:)
  , has
  ) where

import           Graql.Property
import           Graql.Util     (Convert (convert), spaces, with)

-- |A pattern to find in the graph
data Pattern =
  Pattern (Maybe VarOrName)
          [Property]

-- |Create an anonymous variable
var_ :: Pattern
var_ = Pattern Nothing []

-- |Specify a property has a particular type
isa :: (Convert p Pattern, Convert a VarOrName) => p -> a -> Pattern
patt `isa` x = addProperty patt (Isa $ convert x)

-- |Specify a property is a relation between other variables
(<:) :: (Convert p Pattern, Convert a RolePlayer) => p -> [a] -> Pattern
patt <: cs = addProperty patt (Rel $ map convert cs)

-- |Specify a property has a resource
has ::
     (Convert p Pattern, Convert a (Either Value Var))
  => p
  -> Name
  -> a
  -> Pattern
has patt rt v = addProperty patt (Has rt $ convert v)

showProps :: Show a => [a] -> String
showProps = spaces . reverse

addPropToPattern :: Pattern -> Property -> Pattern
addPropToPattern (Pattern n props) prop = Pattern n (prop : props)

addProperty :: Convert a Pattern => a -> Property -> Pattern
addProperty = addPropToPattern . convert

instance Show Pattern where
  show (Pattern v [])    = v `with` "" ++ ";"
  show (Pattern v props) = v `with` " " ++ showProps props ++ ";"

instance Convert Var Pattern where
  convert = convert . (convert :: Var -> VarOrName)

instance Convert Name Pattern where
  convert = convert . (convert :: Name -> VarOrName)

instance Convert VarOrName Pattern where
  convert v = Pattern (Just v) []
