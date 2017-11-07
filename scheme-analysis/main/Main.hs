
import Scheme.Parse
import Scheme.Types
import Interpreter.Evaluate
import Analysis.AbstractInterpreter.Interpreter as AI
import Analysis.StaticAnalysis.Analysis as SA
import Analysis.StaticAnalysis.AnalysisForms
import Analysis.StaticAnalysis.Patterns

import System.Environment
import Control.Monad
import Data.List
import Text.Groom

getText :: [String] -> IO String
getText ["--file", f] = readFile f
getText ss = return $ intercalate "" ss


main :: IO ()
main = do
  text <- join $ getText <$> getArgs
  case runParse text of
    Left perr -> do
      putStrLn "Error parsing!"
      print perr
    Right p@(Program _) -> do
      putStrLn "Evaluating program."
      -- putStrLn "Raw program:"
      -- print p
      -- print "Pretty:"
      -- putStrLn $ join (fmap displayForm frms)
      val <- execEval p
      print val

      putStrLn "Abstract analysis:"
      print $ AI.execAnalysis p

      putStrLn "Analysis"
      let (tProg, parseState) = translateProgram p
      let (analysisStr, analysisState) = SA.execAnalysisStr tProg parseState
      -- putStrLn $ groom analysisState
      -- putStrLn ""
      -- putStrLn ""
      putStrLn analysisStr
      -- putStrLn ""
      -- putStrLn ""
      print (runTest tProg)

      -- putStrLn "Result of analysis:"
      -- print $ runDataFlow first
