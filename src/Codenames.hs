module Codenames where

import Graql.Shell
import Graql.Query

import Data.String
import Data.List
import System.IO
import qualified Data.Map as Map

arcHeader = "stimulus response weight"
verticesHeader = "id name"

parseNet :: [String] -> ([String], [String])
parseNet str = (tail vertices, tail arcs) where
    body = dropWhile (not . isPrefixOf "*Vertices") str
    (vertices, arcs) = break (isPrefixOf "*Arcs") body

splitNetFile :: FilePath -> FilePath -> FilePath -> IO ()
splitNetFile inFile outVertices outArcs = do
    result <- lines <$> readFile inFile
    let (vertices, arcs) = parseNet result
    writeFile outVertices (unlines (verticesHeader : vertices))
    writeFile outArcs (unlines (arcHeader : arcs))

splitEAT :: IO ()
splitEAT = splitNetFile "EATnew/EATnew.net" "vertices.csv" "arcs.csv"

loadSchema :: IO ()
loadSchema = runFile "schema.gql"

migrateWords :: IO ()
migrateWords = migrateCsv "vertices.csv" "verticesTemplate.gql" " "

migrateAssociations :: IO ()
migrateAssociations = migrateCsv "arcs.csv" "arcsTemplate.gql" " "

loadEAT :: IO ()
loadEAT = do
    splitEAT
    loadSchema
    migrateWords
    migrateAssociations

findAssociateWords :: [String] -> IO (Either Error [Value])
findAssociateWords words = do
    results <- runMatch $ queryAssociateWords words
    return $ getValues n results

queryAssociateWords :: [String] -> MatchQuery
queryAssociateWords words = distinct $ match patterns `select` [n]
    where patterns = (x <: [isa word, hasVar name n]) : concatMap makePatterns words
          makePatterns w = [v <: [isa word, hasString name w], var [rel [rp v, rp x], isa association]]
            where v = '_' : w

getValues :: VarName -> Either Error [Result] -> Either Error [Value]
getValues varName = (maybeEither "No value" . mapM (getValue varName) =<<)

getValues :: VarName -> [Result] -> Maybe [Value]
getValues varName results = mapM getValue results

getValue :: VarName -> Result -> Maybe Value
getValue varName result = Map.lookup varName result >>= value

maybeEither :: e -> Maybe a -> Either e a
maybeEither x Nothing  = Left x
maybeEither _ (Just y) = Right y

x = "x"
n = "n"
name = "name"
word = "word"
association = "association"
