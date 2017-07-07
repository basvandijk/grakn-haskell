module Env where

import           Data.String.Utils (rstrip)
import           System.Exit       (ExitCode (ExitSuccess))
import           System.Process    (CreateProcess, createProcess, proc,
                                    readCreateProcess,
                                    waitForProcess)

env :: [String] -> CreateProcess
env = proc "features/grakn-spec/env.sh"

envStart :: IO Bool
envStart = do
  (_, _, _, p) <- createProcess (env ["start"])
  code <- waitForProcess p
  return $ code == ExitSuccess

envStop :: IO ()
envStop = do
  (_, _, _, p) <- createProcess (env ["stop"])
  _ <- waitForProcess p
  return ()

envKeyspace :: IO String
envKeyspace = rstrip <$> readCreateProcess (env ["keyspace"]) ""

envInsert :: String -> IO ()
envInsert patterns = do
  (_, _, _, p) <- createProcess (env ["insert", patterns])
  _ <- waitForProcess p
  return ()
