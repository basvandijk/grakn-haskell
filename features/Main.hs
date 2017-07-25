module Main where

import           Env
import qualified Spec
import           System.Exit       (exitSuccess, exitFailure)
import           Test.Hspec.Runner (Summary (Summary), hspecResult)

main :: IO ()
main = do
  started <- envStart
  if started
    then do
      Summary _ numFailures <- hspecResult Spec.spec
      envStop
      if numFailures /= 0 then exitFailure else exitSuccess
    else exitFailure
