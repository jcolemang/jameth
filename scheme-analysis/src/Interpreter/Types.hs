
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Types where

import Scheme.Types
import Scheme.PrimitiveProcedures

import Control.Monad.State
import Control.Monad.Except
import Control.Arrow ( second )
import qualified Data.Aeson as Aeson
import Data.Aeson ( (.=) )
import Data.Text ( pack )

data Value
  = VConst Constant
  | VProc (Closure Annotation Value)
  | VList [Value]
  | VUndefined
  deriving ( Show, Eq )

instance Aeson.ToJSON Value where
  toJSON (VConst (SStr s))      = Aeson.String $ pack $ show s
  toJSON (VConst (SBool True))  = Aeson.String $ pack $ show "#t"
  toJSON (VConst (SBool False)) = Aeson.String $ pack $ show "#f"
  toJSON (VConst (SInt i))      = Aeson.String $ pack $ show i
  toJSON (VConst (SNum n))      = Aeson.String $ pack $ show n
  toJSON (VConst (SSymbol s))   = Aeson.String $ pack $ show s
  toJSON (VConst SVoid)         = Aeson.String $ pack $ show "#<void>"
  toJSON (VProc _)              = Aeson.String . pack $ show "#<closure>"
  toJSON (VList vals)           = Aeson.String . pack $
    "(list " ++ unwords (fmap show vals) ++ ")"

displayValue :: Value -> String
displayValue (VConst c) =
  displayConstant c
displayValue (VProc c) =
  displayClosure c
displayValue (VList vals) =
  let ss = fmap displayValue vals
  in show ss

data EvalError
  = NonProcedure SourcePos Value
  | UndefinedVariable SourcePos String
  | TypeError SourcePos Value
  | ArithError SourcePos
  | WrongNumArgs SourcePos
  | ValueError SourcePos
  deriving ( Show )

instance Aeson.ToJSON EvalError where
  toJSON (NonProcedure (SP line col) _) =
    Aeson.object
    [ "error"  .= ("Tried to apply a non-procedure" :: Aeson.Value)
    , "line"   .= show line
    , "column" .= show col
    ]
  toJSON (UndefinedVariable (SP line col) val) =
    Aeson.object
    [ "error"  .= (Aeson.String . pack $ ("Variable is not defined: " ++ val))
    , "line"   .= show line
    , "column" .= show col
    ]
  toJSON (TypeError (SP line col) _) =
    Aeson.object
    [ "error"  .= ("There was a type error" :: Aeson.Value)
    , "line"   .= show line
    , "column" .= show col
    ]
  toJSON (ArithError (SP line col)) =
    Aeson.object
    [ "error"  .= ("Arithmetic error" :: Aeson.Value)
    , "line"   .= show line
    , "column" .= show col
    ]
  toJSON (WrongNumArgs (SP line col)) =
    Aeson.object
    [ "error"  .= ("Wrong number of arguments to procedure" :: Aeson.Value)
    , "line"   .= show line
    , "column" .= show col
    ]
  toJSON (ValueError (SP line col)) =
    Aeson.object
    [ "error"  .= ("There was a value error here" :: Aeson.Value)
    , "line"   .= show line
    , "column" .= show col
    ]

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
             , MonadIO
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

typeError :: SourcePos -> Value -> EvalMonad a
typeError sp val =
  throwError $ TypeError sp val

undefinedVariable :: SourcePos -> String -> EvalMonad a
undefinedVariable sp var =
  throwError $ UndefinedVariable sp var

wrongNumArgs :: SourcePos -> EvalMonad a
wrongNumArgs sp =
  throwError $ WrongNumArgs sp

arithmeticError :: SourcePos -> EvalMonad a
arithmeticError sp =
  throwError $ ArithError sp

valueError :: SourcePos -> EvalMonad a
valueError sp =
  throwError $ ValueError sp

instance Environment EvalMonad Value where
  getLocalEnv    = localEnv  <$> get
  getGlobalEnv   = globalEnv <$> get
  putLocalEnv l  = modify $ \s -> s { localEnv  = l }
  putGlobalEnv g = modify $ \s -> s { globalEnv = g }
