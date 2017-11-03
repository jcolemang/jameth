
module Interpreter.Evaluate where

import Scheme.Types
import Interpreter.PrimProcs
import Interpreter.Types

import Debug.Trace

-- | Top Level Definitions

execEval :: Program -> IO (Either EvalError [Value])
execEval =
  execEvalEnv defaultGlobalEnv

execEvalEnv :: GlobalEnvironment Value -> Program -> IO (Either EvalError [Value])
execEvalEnv env prog =
  runEval env (evaluate prog)

evaluate :: Program -> EvalMonad [Value]
evaluate (Program forms) =
  mapM evaluateForm forms

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
         envVal <- getEnvValueM addr
         case envVal of
           Nothing ->
             undefinedVariable sp s
           Just val ->
             return val
       Quote slist ->
         let slistToVals sl =
               case sl of
                 SList vals ->
                   VList (fmap slistToVals vals)
                 Constant c ->
                   VConst c
                 Symbol s ->
                   VConst (SSymbol s)
         in return $ slistToVals slist
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
       Set name addr bodyFrm -> do
         val <- evaluateForm bodyFrm
         putInEnv name val addr
         return $ VConst SVoid

applyClosure :: SourcePos -> Closure Value -> [Value] -> EvalMonad Value
applyClosure sp closure rands =
  case closure of
    Closure (Formals ids) bodies env _ ->
      withNewEnv (zip ids rands) env (last <$> mapM evaluateForm bodies)
    Primitive name _ ->
      applyPrimProc sp name rands
