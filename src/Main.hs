module Main where

import JLParse
import System.Environment
import Control.Monad.Trans.Either
import JLEval
import System.IO


main :: IO ()
main = do
  print "~~~~~~ JAMETH ~~~~~"
  args <- getArgs
  -- let args = ["hello"]
  print args
  trees <- runEitherT $ mapM parseJL args
  print trees
  result <- mapM (mapM evalProgram) trees
  print result
  hFlush stdout
