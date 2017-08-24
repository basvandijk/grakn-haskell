module Env where

import           Data.String.Utils (rstrip)
import           System.Exit       (ExitCode (ExitSuccess))
import           System.Process    (CreateProcess (CreateProcess),
                                    createProcess, readCreateProcess,
                                    waitForProcess)

-- This is an inline of `System.Process.proc` because hfmt gets upset parsing
-- the word `proc` for some reason.
env :: [String] -> CreateProcess
env args =
  CreateProcess
  { cmdspec = RawCommand cmd args
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , detach_console = False
  , create_new_console = False
  , new_session = False
  , child_group = Nothing
  , child_user = Nothing
  , use_process_jobs = False
  }
  where
    cmd = "features/grakn-spec/env.sh"

envStart :: IO Bool
envStart = envSuccess ["start", "0.16.0"]

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
