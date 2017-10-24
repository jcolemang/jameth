
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Types where

import Scheme.Types
import Scheme.PrimitiveProcedures

import Control.Monad.State
import Control.Monad.Except
import Control.Arrow ( second )

data Value
  = VConst Constant
  | VProc (Closure Value)
  | VList [Value]
  | VUndefined
  deriving ( Show, Eq )

data EvalError
  = NonProcedure SourcePos Value
  | UndefinedVariable SourcePos String
  | TypeError SourcePos
  | ArithError SourcePos
  | WrongNumArgs SourcePos
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

defaultGlobalEnv :: GlobalEnvironment Value
defaultGlobalEnv =
  createGlobalEnv (fmap (second VProc) primitiveProcedures)

runEval :: GlobalEnvironment Value -> EvalMonad a -> IO (Either EvalError a)
runEval gEnv m =
  let initialState = EvalState
                     { localEnv = createEmptyEnv
                     , globalEnv = gEnv
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

wrongNumArgs :: SourcePos -> EvalMonad a
wrongNumArgs sp =
  throwError $ WrongNumArgs sp

arithmeticError :: SourcePos -> EvalMonad a
arithmeticError sp =
  throwError $ ArithError sp

instance Environment EvalMonad Value where
  getLocalEnv    = localEnv  <$> get
  getGlobalEnv   = globalEnv <$> get
  putLocalEnv l  = modify $ \s -> s { localEnv  = l }
  putGlobalEnv g = modify $ \s -> s { globalEnv = g }
