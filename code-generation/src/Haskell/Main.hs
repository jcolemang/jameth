module Main where

import CodeGeneration.EvalFile

import System.Environment

main :: IO ()
main =
  let numInBatch = 50
  in do
    args <- getArgs
    prog <- case args of
              [inPath, outPath] ->
                return $ Just (inPath, outPath)
              _ ->
                return Nothing
    case prog of
      Nothing -> do
        putStrLn "Invalid arguments given."
        putStrLn "Please enter arguments as <input-file> <output-file>"
      Just (inFile, outFile) -> do
        putStrLn $ "Streaming results of " ++ inFile ++ " to " ++ outFile
        execFile numInBatch inFile outFile
        putStrLn "Complete"
