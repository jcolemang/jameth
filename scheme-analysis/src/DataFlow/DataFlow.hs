
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataFlow.DataFlow where

import Scheme.Types
import DataFlow.Types
import DataFlow.PrimProcs

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Debug.Trace

execAnalysis :: Program -> Either AnalysisError AbstractValue
execAnalysis prog =
  let initS = initialState prog
  in fst $ runIdentity (runStateT (runExceptT (runAnalysis (allPossibleValues prog)))
                                  initS)

allPossibleValues :: Program -> AnalysisMonad AbstractValue
allPossibleValues (Program forms) =
  last <$> mapM iteration forms

applyAbstClosure :: AbstractValue
                 -> [AbstractValue]
                 -> AnalysisMonad AbstractValue
applyAbstClosure p vals = do
  out <- getOutput
  undefined

runApplyAbstClosure :: AbstractValue
                    -> [AbstractValue]
                    -> AnalysisMonad AbstractValue
runApplyAbstClosure (AbstProc (Closure (Formals ids) bs env _)) rands =
  withNewEnv (zip ids rands) env (last <$> mapM iteration bs)
runApplyAbstClosure (AbstProc (Primitive name arity)) rands =
  applyPrimProc name rands
runApplyAbstClosure (Branch a b) rands = do
  aVal <- applyAbstClosure a rands
  bVal <- applyAbstClosure b rands
  return $ createBranch aVal bVal
runApplyAbstClosure _ _ =
  return (AbstErr NotAProcedure)

iteration :: Form -> AnalysisMonad AbstractValue
iteration (A ann (Const (SStr _))) =
  putOutput (label ann) AbstStr
iteration (A ann (Const (SBool _))) =
  putOutput (label ann) AbstBool
iteration (A ann (Const (SInt _))) =
  putOutput (label ann) AbstNum
iteration (A ann (Const (SNum _))) =
  putOutput (label ann) AbstNum
iteration (A _ (Const _)) =
  error "Constant not yet implemented"
iteration (A ann (Var name addr)) = do
  mVal <- getEnvValue addr
  case mVal of
    Just val ->
      putOutput (label ann) val
    Nothing ->
      putOutput (label ann) (AbstErr UnboundVar)

iteration (A ann (TwoIf _ true false)) = do
  trueBranch <- iteration true
  falseBranch <- iteration false
  putOutput (label ann) (createBranch trueBranch falseBranch)
iteration (A ann (Lambda fs bs)) = do
  let sp = pos ann
  env <- getLocalEnv
  putOutput (label ann) (AbstProc $ Closure fs bs env sp)
iteration f@(A _ (App rator rands)) = do
  ratorVal <- iteration rator
  randVals <- mapM iteration rands
  out <- applyAbstClosure ratorVal randVals
  return out
  -- case ratorVal of
  --   AbstProc c ->
  --   Branch a b ->
  --     undefined
  --   _ -> do
  --     traceShowM $ "Not a procedure: " ++ show f ++ " " ++ show ratorVal
  --     return . AbstErr $ NotAProcedure
iteration f =
  error $ "Not yet implemented: " ++ show f
