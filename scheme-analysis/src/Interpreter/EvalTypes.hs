
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Types where

import Scheme.Types

import Control.Monad.State
import Control.Monad.Except

data EvalError
  = Placeholder

data EvalState
  = EvalState
  { localEnv :: LocalEnvironment Value
  , globalEnv :: GlobalEnvironment Value
  }

newtype EvalMonad a
  = EvalMonad
  { runEval :: ExceptT EvalError (StateT EvalState IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState EvalState
             , MonadError EvalError
             )

instance Environment EvalMonad Value where
  getLocalEnv    = localEnv  <$> get
  getGlobalEnv   = globalEnv <$> get
  putLocalEnv l  = modify $ \s -> s { localEnv  = l }
  putGlobalEnv g = modify $ \s -> s { globalEnv = g }
