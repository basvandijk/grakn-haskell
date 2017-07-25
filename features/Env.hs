module Env where

import           Data.String.Utils (rstrip)
import           System.Exit       (ExitCode (ExitSuccess))
import           System.Process    (CreateProcess, createProcess, proc,
                                    readCreateProcess, waitForProcess)

env :: [String] -> CreateProcess
env = proc "features/grakn-spec/env.sh"

envStart :: IO Bool
envStart = envSuccess ["start", "0.15.0"]

envStop :: IO ()
envStop = do
  _ <- envSuccess ["stop"]
  return ()

envKeyspace :: IO String
envKeyspace = rstrip <$> readCreateProcess (env ["keyspace"]) ""

envInsert :: String -> IO ()
envInsert patterns = do
  _ <- envSuccess ["insert", patterns]
  return ()

envCheckType :: String -> IO Bool
envCheckType label = envSuccess ["check", "type", label]

envCheckInstance :: String -> String -> IO Bool
envCheckInstance res value = envSuccess ["check", "instance", res, value]

envSuccess :: [String] -> IO Bool
envSuccess args = do
    (_, _, _, p) <- createProcess $ env args
    code <- waitForProcess p
    return $ code == ExitSuccess
