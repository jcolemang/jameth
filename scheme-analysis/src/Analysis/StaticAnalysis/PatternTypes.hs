
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analysis.StaticAnalysis.PatternTypes where

import Scheme.Types
import Analysis.StaticAnalysis.Types

import Control.Monad.Writer
import Control.Monad.Identity


type PatternMatcher = RawAnalysisForm -> PatternMonad ()

data LogMessage
  = LogWarning String
  | LogError String
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

logWarning :: String -> PatternMonad ()
logWarning message =
  tell [LogWarning message]

logError :: String -> PatternMonad ()
logError message =
  tell [LogError message]
