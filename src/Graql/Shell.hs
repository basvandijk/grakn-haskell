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
                                     eitherDecodeStrict, parseJSON, (.:?))
import qualified Data.Aeson         as Aeson
import           Data.Map           (Map)
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)
import           Graql.Query
import           System.Process     (callProcess, readProcessWithExitCode)

type Error = String

type Result = Map Var Concept

data Concept = Concept { cid :: Id, ctype :: Maybe Id, value :: Maybe Value }
  deriving Show

instance FromJSON Id where
  parseJSON (Aeson.String s) = return $ gid s

instance FromJSON Concept where
  parseJSON (Aeson.Object obj) =
    Concept <$> (obj Aeson..: "id") <*> (obj .:? "isa") <*> (obj .:? "value")

instance FromJSON Var where
  parseJSON (Aeson.String s) = return $ var s

instance FromJSONKey Var where
  fromJSONKey = FromJSONKeyText var

instance FromJSON Value where
  parseJSON (Aeson.String s) = return $ ValueString s
  parseJSON (Aeson.Number n) = return $ ValueNumber n
  parseJSON (Aeson.Bool   b) = return $ ValueBool b

runFile :: FilePath -> IO ()
runFile path = callProcess "graql.sh" ["-f", path]

migrateCsv :: FilePath -> FilePath -> String -> IO ()
migrateCsv file template delimiter =
  callProcess "migration.sh" args
  where args = ["csv", "-f", file, "-t", template, "-d", delimiter, "-b", "25"]

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
