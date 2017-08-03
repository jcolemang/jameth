module Main where

import JLParse
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  putStrLn . unlines $ map (show . parseJL) args
