
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Types where

import Scheme.Types

import Control.Monad.State
import Control.Monad.Except

data EvalError
  = NonProcedure SourcePos Value
  | UndefinedVariable SourcePos String
  | TypeError SourcePos
  deriving ( Show )

data EvalState
  = EvalState
  { localEnv :: LocalEnvironment Value
  , globalEnv :: GlobalEnvironment Value
  }

newtype EvalMonad a
  = EvalMonad
  { eval :: ExceptT EvalError (StateT EvalState IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState EvalState
             , MonadError EvalError
             )

runEval :: EvalMonad a -> IO (Either EvalError a)
runEval m =
  let initialState = EvalState
                     { localEnv = createEmptyEnv
                     , globalEnv = createEmptyGlobalEnv
                     }
  in fst <$> runStateT (runExceptT (eval m)) initialState


nonProcedure :: SourcePos -> Value -> EvalMonad a
nonProcedure sp v =
  throwError $ NonProcedure sp v

typeError :: SourcePos -> EvalMonad a
typeError sp =
  throwError $ TypeError sp

undefinedVariable :: SourcePos -> String -> EvalMonad a
undefinedVariable sp var =
  throwError $ UndefinedVariable sp var

instance Environment EvalMonad Value where
  getLocalEnv    = localEnv  <$> get
  getGlobalEnv   = globalEnv <$> get
  putLocalEnv l  = modify $ \s -> s { localEnv  = l }
  putGlobalEnv g = modify $ \s -> s { globalEnv = g }
