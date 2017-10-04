
module Analysis.Analysis
  ( analyze
  , formAnalysis
  )
where

import Analysis.AnalysisTypes
import Analysis.Analyzers.AppendAnalysis
import Scheme.JLTypes

programAnalysis :: Program -> [LogMessage]
programAnalysis p =
  undefined


formRecurse :: (Form -> AnalysisMonad ()) -> Form -> AnalysisMonad ()
formRecurse f x@(Value _ _) =
  f x
formRecurse f x@(JLVar {}) =
  f x
formRecurse f x@(JLQuote _ _) =
  f x
formRecurse f x@(JLLambda _ forms _) = do
  f x
  mapM_ (formRecurse f) forms
formRecurse f x@(JLLet asgns forms _) = do
  f x
  mapM_ (formRecurse f . snd) asgns
  mapM_ (formRecurse f) forms
formRecurse f x@(JLTwoIf f1 f2 f3 _) = do
  f x
  mapM_ (formRecurse f) [f1, f2, f3]
formRecurse f x@(JLOneIf f1 f2 _) = do
  f x
  mapM_ (formRecurse f) [f1, f2]
formRecurse f x@(JLDefine _ form _) = do
  f x
  formRecurse f form
formRecurse f x@(JLApp f1 fs _) = do
  f x
  mapM_ (formRecurse f) (f1:fs)



formAnalysis :: Program -> AnalysisMonad ()
formAnalysis (Program forms) =
  let analyzers =
        [ appendAnalysis
        ]
      checkForm f = mapM_ ($ f) analyzers
  in mapM_ (formRecurse checkForm) forms
