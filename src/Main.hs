module Main where

import Interpreter.JLReadSource
import Scheme.JLEvaluationTypes
import Analysis.Analysis
import Analysis.AnalysisTypes

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
      print prog
      analysis <- runEvalT $ formAnalysis prog
      mapM_ print analysis
      -- return ()
  hFlush stdout
