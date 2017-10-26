module Main where

import CodeGeneration.EvalFile

import System.Environment

main :: IO ()
main = do
  path <- head <$> getArgs
  ps <- readFile path
  results <- getResults ps
  putStrLn $ writeResults results
