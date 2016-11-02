{-# LANGUAGE OverloadedStrings #-}

module Graql.Query where

import Data.List (intercalate)
import Data.String (IsString, fromString)
import           Data.Scientific    (Scientific)
import           Data.Text          (Text, pack)

data Value = String Text | Number Scientific | Bool Bool

type VarName = String

data VarOrId = Var VarName | Id String

data MatchQuery = MatchQueryBase Pattern
                | MatchQuerySelect MatchQuery [VarName]
                | MatchQueryLimit MatchQuery Integer
                | MatchQueryDistinct MatchQuery

data Pattern = VarPattern (Maybe VarOrId) [Property] | Disjunction Pattern Pattern | Conjunction [Pattern]

data Property = Isa VarOrId
              | IdProperty String
              | Rel [Casting]
              | Has String (Either Value VarName)

data Casting = Casting (Maybe VarOrId) VarOrId

instance Show VarOrId where
    show (Var varName) = "$" ++ varName
    show (Id string) = string

instance Show MatchQuery where
    show (MatchQueryBase (Conjunction patterns)) = "match " ++ interList " " patterns
    show (MatchQueryBase pattern               ) = "match " ++ show pattern
    show (MatchQuerySelect mq vars             ) = show mq ++ " select " ++ intercalate ", " (map ('$':) vars) ++ ";"
    show (MatchQueryLimit mq limit             ) = show mq ++ " limit " ++ show limit ++ ";"
    show (MatchQueryDistinct mq                ) = show mq ++ " distinct;"

instance Show Pattern where
    show (VarPattern (Just varOrId) properties) = show varOrId ++ " " ++ interList ", " properties ++ ";"
    show (VarPattern Nothing        properties) = interList ", " properties ++ ";"
    show (Disjunction left right) = show left ++ " or " ++ show right ++ ";"
    show (Conjunction patterns) = "{" ++ interList " " patterns ++  "};"

instance Show Property where
    show (Isa varOrId) = "isa " ++ show varOrId
    show (IdProperty string) = "id " ++ string
    show (Rel castings) = "(" ++ interList ", " castings ++ ")"
    show (Has rt (Left value)) = "has " ++ rt ++ " " ++ show value
    show (Has rt (Right varName)) = "has " ++ rt ++ " $" ++ varName

instance Show Casting where
    show (Casting (Just roletype) roleplayer) = show roletype ++ ": " ++ show roleplayer
    show (Casting Nothing         roleplayer) = show roleplayer

instance Show Value where
    show (String text) = show text
    show (Number num) = show num
    show (Bool bool) = show bool

instance IsString Casting where
    fromString = rp

interList :: Show a => String -> [a] -> String
interList sep = intercalate sep . map show

match :: [Pattern] -> MatchQuery
match = MatchQueryBase . Conjunction

select :: MatchQuery -> [VarName] -> MatchQuery
select = MatchQuerySelect

limit :: MatchQuery -> Integer -> MatchQuery
limit = MatchQueryLimit

distinct :: MatchQuery -> MatchQuery
distinct = MatchQueryDistinct

isa :: String -> Property
isa = Isa . Id

rel :: [Casting] -> Property
rel = Rel

has :: String -> Value -> Property
has rt value = Has rt (Left value)

hasString :: String -> String -> Property
hasString rt value = Has rt (Left (String (pack value)))

hasVar :: String -> VarName -> Property
hasVar rt varName = Has rt (Right varName)

(.:) :: String -> VarName -> Casting
rt .: rp = Casting (Just (Id rt)) (Var rp)

rp :: VarName -> Casting
rp = Casting Nothing . Var

var :: [Property] -> Pattern
var = VarPattern Nothing

(<:) :: String -> [Property] -> Pattern
var <: ps = VarPattern (Just (Var var)) ps
