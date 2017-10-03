
module Analysis.Analyzers.AppendAnalysis where

import Analysis.AnalysisTypes
import Scheme.JLTypes

appendAnalysis :: JLForm -> AnalysisMonad ()
appendAnalysis x@(JLApp (JLVar "append" asp)
                (JLApp (JLVar "list" _)
                 [first] _:rest@_) sp) =
  let betterVersion =
        JLApp (JLVar "cons" asp) (first:rest) sp
      message = "Instead of `" ++ displayForm x ++
                "`, why not write `" ++ displayForm betterVersion ++ "`?"
  in tell [Warning message sp]
appendAnalysis _ =
  return ()
