
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

type PrimitiveProcedure = [Value] -> EvaluationMonad Value

data EvaluationError
  = JLEvalError         SourcePos
  | JLUndefined         SourcePos
  | JLTypeError         SourcePos
  | JLNotAProcedure     SourcePos
  | JLBadNumArgs        SourcePos SourcePos
  deriving (Show)

data EvaluationState
  = EvalState
  { localEnv :: LocalEnvironment Value
  , globalEnv :: GlobalEnvironment Value
  } deriving (Show)
