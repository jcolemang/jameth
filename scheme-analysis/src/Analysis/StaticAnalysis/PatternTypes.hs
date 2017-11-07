
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analysis.StaticAnalysis.PatternTypes where

import Scheme.Types
import Analysis.StaticAnalysis.Types

import Control.Monad.Writer
import Control.Monad.Identity


type PatternMatcher = RawAnalysisForm -> PatternMonad ()

data LogMessage
  = LogWarning SourcePos String
  | LogError SourcePos String
  deriving ( Show
           )

type Log = [LogMessage]

newtype PatternMonad a =
  PatternMonad
  { runPatterns :: WriterT Log Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter Log
             )

logWarning :: SourcePos -> String -> PatternMonad ()
logWarning sp message =
  tell [LogWarning sp message]

logError :: SourcePos -> String -> PatternMonad ()
logError sp message =
  tell [LogError sp message]
