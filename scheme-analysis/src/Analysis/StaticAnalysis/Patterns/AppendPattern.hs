
module Analysis.StaticAnalysis.Patterns.AppendPattern where

import Scheme.Types hiding ( Closure )
import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.PatternTypes

import Data.Set as S
import Control.Monad.Writer

import Debug.Trace
import Text.Groom

primWithName :: String -> Type -> Bool
primWithName name (Closure (StaticPrimitive name' _)) = name == name'
primWithName _ _ = False

isOnly :: Show a => Set a -> (a -> Bool) -> Bool
isOnly s f =
  let res = S.filter f s
  in trace (S.showTree s) S.size s == S.size res && S.size s > 0

appendPattern :: PatternMatcher
appendPattern frm =
  case frm of
    AnalysisApp _
      appendF@(A _ AnalysisVar {})
      [A _ (AnalysisApp _
             listF@(A _ AnalysisVar {})
              [  _ ]), _ ] ->
      when (getTypes listF `isOnly` primWithName "list" &&
            getTypes appendF `isOnly` primWithName "append")
           (logWarning "Use cons!")
    _ ->
      return ()
