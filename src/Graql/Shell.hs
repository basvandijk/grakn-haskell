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

import           Control.Applicative (empty)
import           Data.Aeson         (FromJSON, eitherDecodeStrict, parseJSON,
                                     (.:), (.:?))
import qualified Data.Aeson         as Aeson
import           Data.Map           (Map)
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)
import           Graql              hiding ((.:))
import           System.Process     (callProcess, readProcessWithExitCode)

-- |A concept in the graph
data Concept = Concept { cid :: Id, ctype :: Maybe Id, value :: Maybe Value }
  deriving Show

-- |A result of a match query, binding variables to concepts
type Result = Map Var Concept

-- |Run the given file path on the database, ignoring the output
runFile :: FilePath -> IO ()
runFile path = callProcess "graql.sh" ["-f", path]

-- |Run a match query on the graph
runMatch :: MatchQuery -> IO [Result]
runMatch q = do
  result <- parseResults <$> runGraql q
  either fail return result

-- |Run the CSV migrator using the given csv file, template file and separator
migrateCsv :: FilePath -> FilePath -> String -> IO ()
migrateCsv file template separator =
  callProcess "migration.sh" args
  where args = ["csv", "-i", file, "-t", template, "-s", separator]


type Error = String

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

instance FromJSON Concept where
  parseJSON (Aeson.Object obj) =
    Concept <$> (obj .: "id") <*> (obj .:? "isa") <*> (obj .:? "value")
  parseJSON _ = empty
