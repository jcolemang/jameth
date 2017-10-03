
module Analysis.Analysis
  ( analyze
  , formAnalysis
  )
where

import Analysis.AnalysisTypes
import Analysis.Analyzers.AppendAnalysis
import Scheme.JLTypes

programAnalysis :: JLProgram -> [LogMessage]
programAnalysis p =
  undefined

formAnalysis :: JLProgram -> AnalysisMonad ()
formAnalysis (JLProgram forms) =
  let analyzers =
        [ appendAnalysis
        ]
  in mapM_ (`mapM` forms) analyzers
