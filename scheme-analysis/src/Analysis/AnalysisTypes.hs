
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analysis.AnalysisTypes
  ( tell
  , AnalysisMonad (..)
  , LogMessage (..)
  , analyze
  )
where

import Scheme.Types

import Control.Monad.Writer
import Control.Monad.Identity

data LogMessage
  = Warning String SourcePos
  | Error String SourcePos
  deriving (Show)

newtype AnalysisMonad a
  = AnalysisMonad
  { runAnalysis :: WriterT [LogMessage] Identity a
  } deriving (Functor, Applicative, Monad,
              MonadWriter [LogMessage])

analyze :: AnalysisMonad a -> [LogMessage]
analyze am =
  let (_, logs) = runIdentity (runWriterT (runAnalysis am))
  in logs
