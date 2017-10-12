
module Analysis.Analyzers.AppendAnalysis where

import Analysis.AnalysisTypes
import Scheme.JLTypes

-- appendAnalysis :: Form -> AnalysisMonad ()
-- appendAnalysis x@(JLApp (Var "append" appendAddr asp)
--                 (JLApp (Var "list" listAddr _)
--                  [first] _:rest@_) sp) =
--   let betterVersion =
--         JLApp (Var "cons" (globalReference "cons") asp) (first:rest) sp
--       message = "Instead of `" ++ displayForm x ++
--                 "`, why not write `" ++ displayForm betterVersion ++ "`?"
--   in tell [Warning message sp]
-- appendAnalysis _ =
--   return ()
