module Main where

import Scheme.JLParse

import System.Environment
import System.IO


main :: IO ()
main = do
  print "~~~~~~ JAMETH ~~~~~"
  args <- getArgs
  print args
  code <- if head args == "--file"
          then (:[]) <$> readFile (args !! 1)
          else return args
  print "Parsing."
  let trees = runJLParse (unlines code)
  print trees
  -- case trees of
  --   Left err ->
  --     print err
  --   Right ts -> do
  --     print "Parsing!"
  --     result <- mapM runJLParse ts
  --     print $ fmap fst result
      -- result <- mapM (mapM evalProgram) trees
      -- print result
  hFlush stdout
