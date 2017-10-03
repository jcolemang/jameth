
module Analysis.Analyzers.AppendAnalysis where

import Analysis.AnalysisTypes
import Scheme.JLTypes

appendAnalysis :: JLForm -> AnalysisMonad ()
appendAnalysis (JLApp (JLVar "append" _)
                (JLApp (JLVar "list" _)
                 (_:_) _:_) sp) =
  tell [Warning $ "Try just writing cons at position " ++ show sp]
appendAnalysis _ =
  return ()
