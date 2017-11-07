
module Analysis.StaticAnalysis.Patterns.EtaPattern where

import Scheme.Types hiding ( Closure )
import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.PatternTypes
import Analysis.StaticAnalysis.Patterns.Helpers

import Control.Monad.Writer

getVarNames :: [AnalysisForm] -> Maybe [String]
getVarNames [] = Just []
getVarNames (A _ (AnalysisVar name _ _):rest) =
  (name:) <$> getVarNames rest
getVarNames _ = Nothing

etaPattern :: PatternMatcher
etaPattern frm =
  case frm of
    AnalysisLambda _
      (AnalysisFormals m)
      [ A _ (AnalysisApp _ func rands) ] ->
      let varNames = getVarNames rands
      in when (varNames == Just (fmap fst m) &&
               (getTypes func `isOnly` aProc))
         (logWarning "Eta reduction!")
    _ ->
      return ()
