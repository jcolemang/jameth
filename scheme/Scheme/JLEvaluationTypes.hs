
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scheme.JLEvaluationTypes where

import Scheme.JLTypes

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

newtype EvaluationMonad a
  = EvalMonad
  { runParser :: ExceptT EvaluationError (StateT EvaluationState Identity) a
  } deriving ( Functor, Applicative, Monad,
               MonadState EvaluationState, MonadError EvaluationError )

type PrimitiveProcedure = [JLValue] -> EvaluationMonad JLValue

data EvaluationError
  = JLEvalError         JLSourcePos
  | JLUndefined         JLSourcePos
  | JLTypeError         JLSourcePos
  | JLNotAProcedure     JLSourcePos
  | JLBadNumArgs        JLSourcePos JLSourcePos
  deriving (Show)

data EvaluationState
  = EvalState
  { localEnv :: LocalEnvironment JLValue
  , globalEnv :: GlobalEnvironment JLValue
  } deriving (Show)
