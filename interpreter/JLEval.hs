
module JLEval where

import Control.Monad
import JLTypes
import Control.Monad.Trans.State
import Control.Monad.Trans.Either


-- | Helpers

booleanValue :: JLValue -> Bool
booleanValue (JLBool False) = False
booleanValue _ = False

initialState :: EvaluationState
initialState = NothingYet

applyClosure :: JLValue -> [JLValue] -> Either EvaluationError JLValue
applyClosure = undefined


-- | Actual Evaluation

evalProgram :: (Monad m) => JLProgram -> m (Either EvaluationError JLValue)
evalProgram (JLProgram forms) =
  let evaledForms = foldM (\_ next -> evalForm next) JLVoid forms
  in flip evalStateT initialState . runEitherT $ evaledForms


evalForm :: (Monad m)
         => JLForm
         -> EitherT EvaluationError (StateT EvaluationState m) JLValue
evalForm (JLFormExp expr) = evalExpression expr


evalExpression :: (Monad m)
               => JLExpression
               -> EitherT EvaluationError (StateT EvaluationState m) JLValue
evalExpression (JLValue val _) =
  return val
evalExpression (JLVar _ _) =
  undefined
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
  case applyClosure f' args' of
    Left e -> left e
    Right val -> return val
