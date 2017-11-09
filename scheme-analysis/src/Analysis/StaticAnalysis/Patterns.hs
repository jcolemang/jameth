

module Analysis.StaticAnalysis.Patterns
  ( runDefaultPatterns
  )
where

import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.Patterns.AppendPattern
import Analysis.StaticAnalysis.Patterns.EtaPattern
import Scheme.Types

import Control.Monad.Writer
import Control.Monad.Identity

testPatterns :: [PatternMatcher]
testPatterns =
  [ appendPattern
  , etaPattern
  ]

runDefaultPatterns :: AnalysisProgram -> Log
runDefaultPatterns (AnalysisProgram fs) =
  runPatternMonad (mapM_ (checkPatterns testPatterns) fs)

runPatternMonad :: PatternMonad a -> Log
runPatternMonad m =
  snd $ runIdentity (runWriterT (runPatterns m))

runAllPatterns :: [PatternMatcher] -> RawAnalysisForm -> PatternMonad ()
runAllPatterns fs rawForm = mapM_ ($ rawForm) fs

checkPatterns :: [PatternMatcher] -> AnalysisForm -> PatternMonad ()
checkPatterns fs (A _ rawForm) =
  let recurLs = mapM_ (checkPatterns fs)
      recur = checkPatterns fs
  in do
    case rawForm of
      AnalysisLambda _ _ bodies ->
        recurLs bodies
      AnalysisDefine _ _ body ->
        recur body
      AnalysisApp _ rator rands -> do
        recur rator
        recurLs rands
      _ ->
        return ()
    runAllPatterns fs rawForm
