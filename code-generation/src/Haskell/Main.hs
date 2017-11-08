module Main where

import CodeGeneration.EvalFile

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  prog <- case args of
            [prog] ->
              return prog
            ["--file", path] ->
              readFile path
            _ ->
              error "Invalid arguments given"
  results <- getResults prog
  putStrLn $ writeResults results
