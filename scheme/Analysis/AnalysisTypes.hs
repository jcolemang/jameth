
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Analysis.AnalysisTypes
  ( AnalysisMonad (..)
  , LogMessage (..)
  , Pattern (..)
  , VarCondition (..)
  , BoundForm (..)
  -- , analyze
  , logWarning
  , logMessage
  -- , runAnalysis
  )
where

import Scheme.JLTypes
import Scheme.JLEvaluationTypes

import Control.Monad.Writer
-- import Control.Monad.Trans.Writer
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except

data LogMessage
  = Warning String SourcePos
  | Error String SourcePos
  | Message String SourcePos
  deriving (Show)

logWarning :: MonadWriter [LogMessage] m
           => String -> SourcePos -> m ()
logWarning msg sp =
  tell [Warning msg sp]

logMessage :: MonadWriter [LogMessage] m
           => String -> SourcePos -> m ()
logMessage msg sp =
  tell [Warning msg sp]

-- data AnalysisState
--   = AnalysisState
--   { localEnv :: LocalEnvironment BoundForm
--   , globalEnv :: GlobalEnvironment BoundForm
--   }

newtype BoundForm = BF Form
  deriving ( Show )

instance Bindable BoundForm where
  emptySlot = BF $ Value Unbound PrimitiveSource
  lambda formals bodies sp = BF $ Lambda formals bodies sp
  ifThenElse test true false sp = BF $ TwoIf test true false sp
  apply (BF p) args sp = BF $ App p (fmap (\(BF a) -> a) args) sp
  void = BF $ Value Void PrimitiveSource

-- type AnalysisMonad a = EvaluationMonadT (WriterT [LogMessage] Identity) BoundForm a

-- newtype AnalysisMonad a
--   = AnalysisMonad
--   { getAnalysis :: EvaluationMonadT (WriterT [LogMessage] Identity) BoundForm a
--   } deriving ( Functor, Applicative, Monad, MonadState (EvaluationState BoundForm)
--              , MonadError EvaluationError
--              , MonadWriter [LogMessage]
--              )
-- analyze :: AnalysisMonad a -> [LogMessage]
-- analyze am =
--   let (_, logs) = runIdentity (runWriterT (runStateT (runAnalysis am) undefined))
--   in logs

-- instance (Monoid w, Monad m) => MonadWriter w (AnalysisMonad w m) where
--   tell = lift . tell
  -- listen = lift . listen
  -- pass = lift . pass

-- instance (Monoid w, Monad m) => MonadWriter w (Strict.WriterT w m) where
--     writer = Strict.writer
--     tell   = Strict.tell
--     listen = Strict.listen
--     pass   = Strict.pass

-- instance (Monoid w) => MonadWriter w (Writer w) where
--     pass   (Writer ((a,f),w)) = Writer (a,f w)
--     listen (Writer (a,w))     = Writer ((a,w),w)
--     tell   s                  = Writer ((),s)

type AnalysisMonad a = EvaluationMonadT Identity BoundForm [LogMessage] a

-- runAnalysis :: EvaluationMonad a
--             -> Either (EvaluationError (a, [LogMessage])
-- runAnalysis am =
--   runEvalT (evalT am)

data VarCondition
  = OriginalGlobal String
  | AnyVar
  | AnyVars

data Pattern
  = AppMatches Pattern [Pattern]
  | MustBe VarCondition
  | Anything
