module Main where

import           Env
import qualified Spec
import           System.Exit       (exitFailure)
import           Test.Hspec.Runner

main :: IO ()
main = do
  started <- envStart
  if started
    then do
      hspecWith defaultConfig Spec.spec
      envStop
    else exitFailure
