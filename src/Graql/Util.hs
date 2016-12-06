module Graql.Util
    ( commas
    , spaces
    ) where

import           Data.List        (intercalate)

commas :: Show a => [a] -> String
commas = interList ", "

spaces :: Show a => [a] -> String
spaces = interList " "

interList :: Show a => String -> [a] -> String
interList sep = intercalate sep . map show
