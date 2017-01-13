module Main where

import Data.Function ((&))
import Test.Hspec
import Graql
import qualified Example

main :: IO ()
main = hspec $ do

    it "a simple query string representation" $
        match [x `isa` person] ~= "match $x isa person;"

    it "a relation query string representation" $
        match [rel [x, y]] ~= "match ($x, $y);"

    it "a relation query string representation with types" $
        match [rel [husband.: x, wife.: y] -: marriage]
            ~= "match (husband: $x, wife: $y) isa marriage;"

    it "a resource query string representation" $
        match [x `hasText` firstName $ "Bob"]
            ~= "match $x has first-name \"Bob\";"

    it "multiple patterns" $
        match [x-:person, y-:firstName]
            ~= "match $x isa person; $y isa first-name;"

    it "select query with type" $
        match [x -: y] & select [x, y]
            ~= "match $x isa $y; select $x, $y;"

    it "mix role types" $
        match [rel [husband.: x, rp y]] ~= "match (husband: $x, $y);"

    it "limit and distinct" $
        match [x -: person] & distinct & limit 10
            ~= "match $x isa person; distinct; limit 10;"

    it "type of type" $
        match [person -: x] ~= "match person isa $x;"

    it "reify a relation" $
        match [x <: [y, z]] ~= "match $x ($y, $z);"

    it "match just a variable" $
        match [x] ~= "match $x;"

    it "example query output" $
        Example.query
            ~= "match $x isa person; (husband: $x, wife: $y) isa marriage; select $y;"


x :: Var
x = var "x"

y :: Var
y = var "y"

z :: Var
z = var "z"

person :: Name
person = name "person"

firstName :: Name
firstName = name "first-name"

marriage :: Name
marriage = name "marriage"

husband :: Name
husband = name "husband"

wife :: Name
wife = name "wife"

infixr 0 ~=

(~=) :: MatchQuery -> String -> Expectation
(~=) = shouldBe . show
