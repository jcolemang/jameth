
module Analysis.StaticAnalysis.Patterns.AppendPattern where

import Scheme.Types
import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.PatternTypes

import Data.Set

appendPattern :: PatternMatcher
appendPattern frm =
  case frm of
    AnalysisApp _ (A _ appendF) [A _ (AnalysisApp _ listF [_])] ->
      if getTypes list `isSubsetOf` empty
      then undefined
      else undefined
    _ ->
      return ()
