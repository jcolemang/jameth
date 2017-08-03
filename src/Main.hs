module Main where

import Parse
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  putStrLn . unlines $ map (show . parseJL) args
