
module Interpreter.Evaluate where

import Scheme.Types
import Interpreter.PrimProcs
import Interpreter.Types

-- | Top Level Definitions

execEval :: Program -> IO (Either EvalError Value)
execEval =
  execEvalEnv defaultGlobalEnv

execEvalEnv :: GlobalEnvironment Value -> Program -> IO (Either EvalError Value)
execEvalEnv env prog =
  runEval env (evaluate prog)

evaluate :: Program -> EvalMonad Value
evaluate (Program forms) = do
  vals <- mapM evaluateForm forms
  return $ last vals

toBool :: Value -> Bool
toBool (VConst (SBool b)) = b
toBool _ = True

-- | Helpers

evaluateForm :: Form -> EvalMonad Value
evaluateForm (A ann f) =
  let sp = pos ann
  in case f of
       Const v ->
         return $ VConst v
       Var s addr -> do
         envVal <- getEnvValue addr
         case envVal of
           Nothing ->
             undefinedVariable sp s
           Just val ->
             return val
       Lambda formals bodies -> do
         l <- getLocalEnv
         return . VProc $ Closure formals bodies l sp
       App ratorForm randForms -> do
         rator <- evaluateForm ratorForm
         rands <- mapM evaluateForm randForms
         case rator of
           VProc closure ->
             applyClosure sp closure rands
           v ->
             nonProcedure sp v
       Define name body -> do
         putInGlobalEnv name VUndefined
         val <- evaluateForm body
         putInGlobalEnv name val
         return $ VConst SVoid
       TwoIf test true false -> do
         direction <- evaluateForm test
         if toBool direction
         then evaluateForm true
         else evaluateForm false
       v ->
         error $ "Not yet implemented: " ++ show v

applyClosure :: SourcePos -> Closure Value -> [Value] -> EvalMonad Value
applyClosure sp closure rands =
  case closure of
    Closure (Formals ids) bodies env _ ->
      withNewEnv (zip ids rands) env (last <$> mapM evaluateForm bodies)
    Primitive name _ ->
      applyPrimProc sp name rands
