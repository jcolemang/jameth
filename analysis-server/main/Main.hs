
import Server

import System.Environment
import Path

main :: IO ()
main = do
  args <- getArgs
  case args of
    [p] ->
      case parseRelFile p of
        Left _ ->
          putStrLn "Argument must specify path to index.html"
        Right indexFile ->
          runServer indexFile
    _ -> do
      putStrLn "Exactly one argument is required."
      putStrLn "Must specify path of index.html"
