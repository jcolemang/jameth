
module Interpreter.Evaluate where

import Scheme.Types
import Interpreter.Types
import Interpreter.PrimProcs

import Debug.Trace

-- | Top Level Definitions

execEval :: Program -> IO (Either EvalError Value)
execEval prog =
  runEval (evaluate prog)

evaluate :: Program -> EvalMonad Value
evaluate (Program forms) = do
  vals <- mapM evaluateForm forms
  return $ last vals

-- | Helpers

evaluateForm :: Form -> EvalMonad Value
evaluateForm (A ann f) =
  let sp = pos ann
  in case f of
       Value v ->
         return v
       Var s addr -> do
         envVal <- getEnvValue addr
         case envVal of
           Nothing ->
             undefinedVariable sp s
           Just val ->
             return val
       Lambda formals bodies -> do
         l <- getLocalEnv
         return . Proc $ Closure formals bodies l sp
       App ratorForm randForms -> do
         rator <- evaluateForm ratorForm
         rands <- mapM evaluateForm randForms
         -- traceShowM $ "Op: " ++ show rator ++ "   Rands: " ++ show rands
         case rator of
           Proc closure ->
             applyClosure sp closure rands
           v ->
             nonProcedure sp v
       Define name body -> do
         putInGlobalEnv name Undefined
         val <- evaluateForm body
         putInGlobalEnv name val
         return $ Const SVoid
       _ ->
         error "Not yet implemented"

applyClosure :: SourcePos -> Closure -> [Value] -> EvalMonad Value
applyClosure sp closure rands =
  case closure of
    Closure (Formals ids) bodies env _ -> do
      curr <- getLocalEnv :: EvalMonad (LocalEnvironment Value)
      putLocalEnv env
      extendEnv $ zip ids rands
      vals <- mapM evaluateForm bodies
      putLocalEnv curr
      return $ last vals
    Primitive name _ ->
      applyPrimProc sp name rands
