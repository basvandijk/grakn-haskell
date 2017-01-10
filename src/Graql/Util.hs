module Graql.Util
    ( with
    , commas
    , spaces
    ) where

import           Data.List        (intercalate)

with :: Show a => Maybe a -> String -> String
(Just val) `with` suffix = show val ++ suffix
Nothing    `with` _      = ""

commas :: Show a => [a] -> String
commas = interList ", "

spaces :: Show a => [a] -> String
spaces = interList " "

interList :: Show a => String -> [a] -> String
interList sep = intercalate sep . map show
