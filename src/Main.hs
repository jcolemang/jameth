module Main where

-- import JLParse
import JLTokenize
import System.Environment
import System.IO
import Control.Monad.Trans.Except


main :: IO ()
main = do
  print "~~~~~~ JAMETH ~~~~~"
  args <- getArgs
  print args
  code <- if head args == "--file"
          then (:[]) <$> readFile (args !! 1)
          else return args
  print "Removed comments"
  print $ map removeAllComments code
  print "Tokenized"
  trees <- runExceptT $ mapM readJL code
  print trees
  -- result <- mapM (mapM evalProgram) trees
  -- print result
  hFlush stdout
