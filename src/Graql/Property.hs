module Graql.Property
    ( Property (..)
    , Var
    , Id
    , VarOrId
    , Value (..)
    , IsVarOrId
    , IsCasting
    , IsResource
    , var
    , gid
    , (.:)
    , rp
    , toVarOrId
    , toCasting
    , toResource
    ) where

import           Graql.Util
import           Data.Text        (Text, unpack)
import           Data.Scientific  (Scientific)
import           Text.Regex.Posix ((=~))
import           Control.Applicative (empty)
import           Data.Aeson       (FromJSON, FromJSONKey,
                                   FromJSONKeyFunction (FromJSONKeyText),
                                   parseJSON)
import qualified Data.Aeson       as Aeson

-- |A property of a concept
data Property = Isa VarOrId
              | IdProperty Id
              | Rel [Casting]
              | Has Id (Either Value Var)

-- |A variable name wildcard that will represent a concept in the results
data Var = Var Text deriving (Eq, Ord)

-- |An ID of something in the graph
data Id = Id Text

-- |Something that may be a variable name or an ID
data VarOrId = VarName Var | IdName Id

-- |A value of a resource
data Value = ValueString Text | ValueNumber Scientific | ValueBool Bool

-- |A casting, relating a role type and role player
data Casting = Casting (Maybe VarOrId) VarOrId

-- |Something that can be converted into a variable or an ID
class IsVarOrId a where
    toVarOrId :: a -> VarOrId

-- |Something that can be converted into a casting
class IsCasting a where
    toCasting :: a -> Casting

-- |Something that can be converted into a resource value or variable
class IsResource a where
    toResource :: a -> Either Value Var

-- |Create a variable
var :: Text -> Var
var = Var

-- |Create an ID of something in the graph
gid :: Text -> Id
gid = Id

-- |A casting in a relation between a role type and a role player
(.:) :: (IsVarOrId a, IsVarOrId b) => a -> b -> Casting
rt .: player = Casting (Just $ toVarOrId rt) (toVarOrId player)

-- |A casting in a relation without a role type
rp :: IsVarOrId a => a -> Casting
rp = Casting Nothing . toVarOrId


idRegex :: String
idRegex = "^[a-zA-Z_][a-zA-Z0-9_-]*$"

instance Show Property where
    show (Isa varOrId         ) = "isa " ++ show varOrId
    show (IdProperty i        ) = "id " ++ show i
    show (Rel castings        ) = "(" ++ commas castings ++ ")"
    show (Has rt (Left  value)) = "has " ++ show rt ++ " " ++ show value
    show (Has rt (Right v    )) = "has " ++ show rt ++ " " ++ show v
    
instance Show Casting where
    show (Casting (Just rt) player) = show rt ++ ": " ++ show player
    show (Casting Nothing   player) = show player

instance Show Value where
    show (ValueString text) = show text
    show (ValueNumber num ) = show num
    show (ValueBool   bool) = show bool

instance Show Id where
    show (Id text)
      | str =~ idRegex = str
      | otherwise      = show text
        where str = unpack text

instance Show Var where
    show (Var v) = '$' : unpack v

instance Show VarOrId where
    show (VarName v) = show v
    show (IdName  i) = show i

instance IsVarOrId Var where
    toVarOrId = VarName

instance IsVarOrId Id where
    toVarOrId = IdName

instance IsCasting Casting where
    toCasting = id

instance IsCasting VarOrId where
    toCasting = Casting Nothing

instance IsCasting Var where
    toCasting = toCasting . toVarOrId

instance IsResource Var where
    toResource = Right

instance IsResource Text where
    toResource = Left . ValueString

instance IsResource Scientific where
    toResource = Left . ValueNumber

instance IsResource Bool where
    toResource = Left . ValueBool

instance FromJSON Value where
  parseJSON (Aeson.String s) = return $ ValueString s
  parseJSON (Aeson.Number n) = return $ ValueNumber n
  parseJSON (Aeson.Bool   b) = return $ ValueBool b
  parseJSON _                = empty

instance FromJSON Id where
  parseJSON (Aeson.String s) = return $ gid s
  parseJSON _                = empty

instance FromJSON Var where
  parseJSON (Aeson.String s) = return $ var s
  parseJSON _                = empty

instance FromJSONKey Var where
  fromJSONKey = FromJSONKeyText var
