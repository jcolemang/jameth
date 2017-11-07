
module Analysis.StaticAnalysis.Patterns.AppendPattern where

import Scheme.Types hiding ( Closure )
import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.PatternTypes
import Analysis.StaticAnalysis.Patterns.Helpers

import Data.Set as S
import Control.Monad.Writer

appendPattern :: PatternMatcher
appendPattern frm =
  case frm of
    AnalysisApp _
      appendF@(A _ AnalysisVar {})
      [A _ (AnalysisApp _
             listF@(A _ AnalysisVar {})
              [ _ ]), _ ] ->
      when (getTypes listF `isOnly` primWithName "list" &&
            getTypes appendF `isOnly` primWithName "append")
           (logWarning "Use cons!")
    _ ->
      return ()
