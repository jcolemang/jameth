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
  print args
  code <- if head args == "--file"
          then (:[]) <$> readFile (args !! 1)
          else return args
  trees <- runEitherT $ mapM parseJL code
  print trees
  result <- mapM (mapM evalProgram) trees
  print result
  hFlush stdout
