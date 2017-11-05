
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.AbstractInterpreter.Interpreter where

import Scheme.Types
import Analysis.AbstractInterpreter.Types
import Analysis.AbstractInterpreter.PrimProcs

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

applyAbstClosure :: Label
                 -> SourcePos
                 -> Closure AbstractValue
                 -> [AbstractValue]
                 -> AnalysisMonad AbstractValue
applyAbstClosure lab sp p vals = do
  out <- getOutput lab vals
  case out of
    Just v ->
      return v
    Nothing ->
      case p of
        Closure (Formals ids) bodies env _ -> do
          numVs <- numVisits lab
          if numVs > 100
            then return Top
            else visit lab >>
                 withNewEnv (zip ids vals)
                            env
                            (last <$> mapM iteration bodies)
        Primitive name _ ->
          applyPrimProc sp name vals

iteration :: Form -> AnalysisMonad AbstractValue
iteration (A _ (Const (SStr _))) =
  return AbstStr
iteration (A _ (Const (SBool _))) =
  return AbstBool
iteration (A _ (Const (SInt _))) =
  return AbstNum
iteration (A _ (Const (SNum _))) =
  return AbstNum
iteration (A _ (Const _)) =
  error "Constant not yet implemented"

iteration (A ann (Var var addr)) = do
  mVal <- getEnvValueM addr
  case mVal of
    Just val ->
      return val
    Nothing ->
      runtimeError (UnboundVar var) (pos ann)

iteration (A _ (Quote (Symbol _))) =
  return AbstSym
iteration (A _ (Quote (Constant (SInt _)))) =
  return AbstNum
iteration (A _ (Quote (Constant (SNum _)))) =
  return AbstNum
iteration (A _ (Quote x@(SList _))) =
  return (slistToAbstVal x)

iteration (A ann (Lambda fs bs)) = do
  let sp = pos ann
  env <- getLocalEnv
  return (AbstProc $ Closure fs bs env sp)

iteration (A _ (TwoIf test true false)) = do
  _ <- iteration test
  trueBranch <- iteration true
  falseBranch <- iteration false
  return $ createBranch trueBranch falseBranch

iteration (A _ (Define s f)) = do
  putInGlobalEnv s AbstUndefined
  val <- iteration f
  putInGlobalEnv s val
  return AbstVoid

iteration (A ann (App rator rands)) = do
  ratorVal <- iteration rator
  randVals <- mapM iteration rands
  case ratorVal of
    AbstProc c ->
      applyAbstClosure (label ann) (pos ann) c randVals
    _ ->
      runtimeError NotAProcedure (pos ann)

iteration f =
  error $ "Not yet implemented: " ++ show f

slistToAbstVal :: SList -> AbstractValue
slistToAbstVal (Symbol _) = AbstSym
slistToAbstVal (Constant (SBool _)) = AbstBool
slistToAbstVal (Constant (SNum _)) = AbstNum
slistToAbstVal (Constant (SInt _)) = AbstNum
slistToAbstVal (SList sl) = AbstList $ fmap slistToAbstVal sl
