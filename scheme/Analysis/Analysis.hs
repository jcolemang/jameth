
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Analysis
  -- ( analyze
  ( formAnalysis
  )
where

import Analysis.AnalysisTypes
import Analysis.Analyzers.AppendAnalysis
import Scheme.JLTypes
import Scheme.JLEvaluationTypes

import Control.Monad.Identity
-- import Scheme.JLEvaluationTypes

-- import Control.Monad.Identity
-- import Control.Monad.State

-- matchesPattern :: LocalEnvironment a
--                -> GlobalEnvironment a
--                -> Pattern
--                -> Form
--                -> Bool
-- matchesPattern p f = undefined


-- analyzeForm :: Form
--             -> AnalysisMonad BoundForm
-- analyzeForm v@Value {} = return $ BF v
-- analyzeForm v@Var {} = return $ BF v
-- analyzeForm v@Quote {} = return $ BF v
-- analyzeForm v@Lambda {} = return $ BF v
-- analyzeForm v@Let {} = return $ BF v
-- analyzeForm v@TwoIf {} = return $ BF v
-- analyzeForm v@OneIf {} = return $ BF v
-- analyzeForm v@Define {} = return $ BF v
-- analyzeForm v@App {} = return $ BF v

-- formRecurse :: (Form -> AnalysisMonad ())
--             -> Form
--             -> AnalysisMonad ()
-- formRecurse f x@Value {} =
--   f x
-- formRecurse f x@Var {} =
--   f x
-- formRecurse f x@Quote {} =
--   f x
-- formRecurse f x@(Lambda _ forms _) = do
--   f x
--   mapM_ (formRecurse f) forms
-- formRecurse f x@(JLLet asgns forms _) = do
--   f x
--   mapM_ (formRecurse f . snd) asgns
--   mapM_ (formRecurse f) forms
-- formRecurse f x@(TwoIf f1 f2 f3 _) = do
--   f x
--   mapM_ (formRecurse f) [f1, f2, f3]
-- formRecurse f x@(OneIf f1 f2 _) = do
--   f x
--   mapM_ (formRecurse f) [f1, f2]
-- formRecurse f x@(JLDefine _ form _) = do
--   f x
--   formRecurse f form
-- formRecurse f x@(App f1 fs _) = do
--   f x
--   mapM_ (formRecurse f) (f1:fs)

-- formAnalysis :: Program -> EvaluationMonadT Identity BoundForm [[BoundForm]]
formAnalysis (Program forms) =
  let analyzers =
        [ appendAnalysis
        ]
      analysisSuite f =
        fmap head (mapM ($ f) analyzers)
      -- checkForm f = mapM_ ($ f) analyzers
  -- in mapM_ (formRecurse checkForm) forms
  in mapM (`evaluate` analysisSuite) forms
