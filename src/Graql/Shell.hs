{-# LANGUAGE OverloadedStrings #-}

module Graql.Shell where

import Graql.Query
import           Data.Aeson         (FromJSON, eitherDecodeStrict, parseJSON, (.:?))
import qualified Data.Aeson         as Aeson
import           Data.Map           (Map)
import           Data.Scientific    (Scientific)
import           Data.Text          (Text, pack)
import           Data.Text.Encoding (encodeUtf8)

import           System.IO          (hGetLine)
import           System.Process     (CreateProcess, proc, callProcess, readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))

type Error = String
type Result = Map VarName Concept

data Concept = Concept {
    cid   :: String,
    ctype   :: Maybe String,
    value :: Maybe Value
} deriving Show

instance FromJSON Concept where
    parseJSON (Aeson.Object obj) = Concept <$> (obj Aeson..: "id") <*> (obj .:? "isa") <*> (obj .:? "value")

instance FromJSON Value where
    parseJSON (Aeson.String s) = return $ String s
    parseJSON (Aeson.Number n) = return $ Number n
    parseJSON (Aeson.Bool b)   = return $ Bool b

runFile :: FilePath -> IO ()
runFile path = callProcess "graql.sh" ["-f", path]

migrateCsv :: FilePath -> FilePath -> String -> IO ()
migrateCsv file template delimiter = callProcess "migration.sh" ["csv", "--file", file, "--template", template, "-d", delimiter]

runMatch :: MatchQuery -> IO (Either Error [Result])
runMatch q = do
    result <- runGraql q
    return $ result >>= parseResults

runGraql :: MatchQuery -> IO (Either Error String)
runGraql q = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "graql.sh" ["-e", show q, "-o", "json"] ""
    return $ if length (lines stderr) <= 1 then Right stdout else Left stderr

parseResults :: String -> Either Error [Result]
parseResults = mapM parseResult . lines

parseResult :: String -> Either Error Result
parseResult = eitherDecodeStrict . encodeUtf8 . pack
