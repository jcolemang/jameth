
import Scheme.Parse
import Scheme.Types
import Interpreter.Evaluate

import System.Environment
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case runParse (head args) of
    Left perr -> do
      putStrLn "Error parsing!"
      print perr
    Right p@(Program fs) -> do
      putStrLn "Evaluating program:"
      print p
      print "Pretty:"
      putStrLn $ join (fmap displayForm fs)
      val <- execEval p
      print val
      -- putStrLn "Result of analysis:"
      -- print $ runDataFlow first
