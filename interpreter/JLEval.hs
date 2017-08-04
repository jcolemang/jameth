
module JLEval where

import Control.Monad
import JLTypes
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Data.Map




-- | Primitive procudures

jlAdd2 (JLInt a) (JLInt b) = return . JLInt $ a + b
jlAdd2 (JLNum a) (JLInt b) = return . JLNum $ a + fromInteger b
jlAdd2 (JLInt a) (JLNum b) = return . JLNum $ fromInteger a + b
jlAdd2 (JLNum a) (JLNum b) = return . JLNum $ a + b
jlAdd2 _ _ = left JLUndefined

jlAdd [] = return $ JLInt 0
jlAdd [x@(JLInt _)] = return x
jlAdd [x@(JLNum _)] = return x
jlAdd nums = foldM jlAdd2 (JLInt 0) nums




-- | Helpers

booleanValue :: JLValue m -> Bool
booleanValue (JLBool False) = False
booleanValue _ = False


initialEnvironment :: (Monad m) => Environment m
initialEnvironment = fromList
  [ ("+", JLProc $ JLClosure jlAdd) ]

initialState :: (Monad m) => EvaluationState m
initialState = EvaluationState
  { _environment = initialEnvironment }


-- applyClosure :: JLClosure m -> [JLValue m] -> EitherT EvaluationError (StateT ()) (JLValue m)
applyClosure (JLProc (JLClosure f)) args = f args


-- | Actual Evaluation

evalProgramT :: (Monad m)
             => JLProgram m
             -> EitherT EvaluationError (StateT (EvaluationState m) m) (JLValue m)
evalProgramT (JLProgram forms) =
  foldM (\_ next -> evalForm next) JLVoid forms

evalProgram :: (Monad m) => JLProgram m -> m (Either EvaluationError (JLValue m))
evalProgram code =
  let result = evalProgramT code
  in flip evalStateT initialState $ runEitherT result




evalForm :: (Monad m)
         => JLForm m
         -> EitherT EvaluationError (StateT (EvaluationState m) m) (JLValue m)
evalForm (JLFormExp expr) = evalExpression expr


evalExpression :: (Monad m)
               => JLExpression m
               -> EitherT EvaluationError (StateT (EvaluationState m) m) (JLValue m)
evalExpression (JLValue val _) =
  return val
evalExpression (JLVar _ _) =
  return . JLProc . JLClosure $ jlAdd
evalExpression (JLQuote _ _) =
  undefined
evalExpression (JLLambda _) =
  undefined
evalExpression (JLTwoIf condexp thenexp elseexp _) = do
  condVal <- evalExpression condexp
  if booleanValue condVal
    then evalExpression thenexp
    else evalExpression elseexp
evalExpression (JLOneIf condexp thenexp _) = do
  condVal <- evalExpression condexp
  if booleanValue condVal
    then evalExpression thenexp
    else return JLVoid
evalExpression (JLApp f args _) = do
  f' <- evalExpression f
  args' <- mapM evalExpression args
  applyClosure f' args'
  -- case applyClosure f' args' of
  --   Left e -> left e
  --   Right val -> return val
