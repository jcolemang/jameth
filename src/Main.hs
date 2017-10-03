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
  prog <- readSourceFile p
  print prog
  case prog of
    err@(Left _) ->
      print err
    Right p -> do
      print "Doing analysis"
      print (analyze $ formAnalysis p)

  hFlush stdout
