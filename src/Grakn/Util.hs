module Grakn.Util
  ( Convert(convert)
  , with
  , commas
  , spaces
  ) where

import           Data.List (intercalate)

class Convert a b where
  convert :: a -> b

instance Convert a a where
  convert = id

with :: Show a => Maybe a -> String -> String
(Just val) `with` suffix = show val ++ suffix
Nothing `with` _ = ""

commas :: Show a => [a] -> String
commas = interList ", "

spaces :: Show a => [a] -> String
spaces = interList " "

interList :: Show a => String -> [a] -> String
interList sep = intercalate sep . map show
