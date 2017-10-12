module Main where

-- import Scheme.JLParse
import Scheme.JLReadSource
import Analysis.Analysis

import System.Environment
import System.IO
import Path
import Data.IORef


main :: IO ()
main = do
  print "~~~~~~ JAMETH ~~~~~"
  args <- getArgs
  print args
  p <- parseRelFile (head args)
  src <- readSourceFile p
  case src of
    err@(Left _) ->
      print err
    Right prog -> do
      print prog
      -- print (analyze $ formAnalysis prog)
  hFlush stdout


test :: IORef Int -> Int -> IO ()
test ref i =
  modifyIORef ref (+i)
