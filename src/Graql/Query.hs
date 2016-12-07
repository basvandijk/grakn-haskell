module Graql.Query
    ( MatchQuery
    , match
    , select
    , limit
    , distinct
    ) where

import           Graql.Util
import           Graql.Pattern
import           Graql.Property

-- |A Graql 'match' query that finds a pattern in the graph
data MatchQuery = Match [Pattern]
                | Select MatchQuery [Var]
                | Limit MatchQuery Integer
                | Distinct MatchQuery

-- |Create a match query by providing a list of patterns
match :: IsPattern a => [a] -> MatchQuery
match = Match . map toPattern

-- |Select variables from a match query, intended to be used infix
select :: [Var] -> MatchQuery -> MatchQuery
select = flip Select

-- |Limit a match query, intended to be used infix
limit :: Integer -> MatchQuery -> MatchQuery
limit = flip Limit

-- |Retrieve only distinct results from a match query
distinct :: MatchQuery -> MatchQuery
distinct = Distinct


instance Show MatchQuery where
    show (Match patts   ) = "match " ++ spaces patts
    show (Select mq vars) = show mq ++ " select " ++ commas vars ++ ";"
    show (Limit mq lim  ) = show mq ++ " limit " ++ show lim ++ ";"
    show (Distinct mq   ) = show mq ++ " distinct;"

