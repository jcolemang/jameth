module Main where

-- import Scheme.JLParse
import Interpreter.JLReadSource
import Analysis.Analysis

import System.Environment
import System.IO
import Path


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
      print "Doing analysis"
      print (analyze $ formAnalysis prog)

  hFlush stdout
