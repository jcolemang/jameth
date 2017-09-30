module Main where

-- import Scheme.JLParse
import Interpreter.JLReadSource

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
  hFlush stdout
