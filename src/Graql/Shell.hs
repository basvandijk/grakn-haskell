module Graql.Shell
    ( Concept
    , Result
    , cid
    , ctype
    , value
    , runFile
    , runMatch
    , migrateCsv
    ) where

import           Data.Aeson         (FromJSON, FromJSONKey,
                                     FromJSONKeyFunction (FromJSONKeyText),
                                     eitherDecodeStrict, parseJSON, (.:), (.:?))
import qualified Data.Aeson         as Aeson
import           Data.Map           (Map)
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)
import           Graql.Query        hiding ((.:))
import           System.Process     (callProcess, readProcessWithExitCode)

type Error = String

-- |A result of a match query, binding variables to concepts
type Result = Map Var Concept

-- |A concept in the graph
data Concept = Concept { cid :: Id, ctype :: Maybe Id, value :: Maybe Value }
  deriving Show

instance FromJSON Id where
  parseJSON (Aeson.String s) = return $ gid s

instance FromJSON Concept where
  parseJSON (Aeson.Object obj) =
    Concept <$> (obj .: "id") <*> (obj .:? "isa") <*> (obj .:? "value")

instance FromJSON Var where
  parseJSON (Aeson.String s) = return $ var s

instance FromJSONKey Var where
  fromJSONKey = FromJSONKeyText var

instance FromJSON Value where
  parseJSON (Aeson.String s) = return $ ValueString s
  parseJSON (Aeson.Number n) = return $ ValueNumber n
  parseJSON (Aeson.Bool   b) = return $ ValueBool b

-- |Run the given file path on the database, ignoring the output
runFile :: FilePath -> IO ()
runFile path = callProcess "graql.sh" ["-f", path]

-- |Run the CSV migrator using the given csv file, template file and separator
migrateCsv :: FilePath -> FilePath -> String -> IO ()
migrateCsv file template separator =
  callProcess "migration.sh" args
  where args = ["csv", "-i", file, "-t", template, "-s", separator]

-- |Run a match query on the graph
runMatch :: MatchQuery -> IO [Result]
runMatch q = do
  result <- parseResults <$> runGraql q
  either fail return result

runGraql :: MatchQuery -> IO String
runGraql q = do
  (_, stdout, stderr) <- readProcessWithExitCode "graql.sh" args ""
  if length (lines stderr) <= 1
    then return stdout
    else fail stderr
  where args = ["-e", show q, "-o", "json"]

parseResults :: String -> Either Error [Result]
parseResults = mapM parseResult . lines

parseResult :: String -> Either Error Result
parseResult = eitherDecodeStrict . encodeUtf8 . pack
