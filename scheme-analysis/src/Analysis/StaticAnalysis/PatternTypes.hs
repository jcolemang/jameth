
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analysis.StaticAnalysis.PatternTypes
  ( Log
  , LogMessage (..)
  , PatternMonad (..)
  , PatternMatcher
  , logWarning
  , logError
  )
where

import Analysis.StaticAnalysis.FormTypes

import Control.Monad.Writer
import Control.Monad.Identity
import Data.Aeson
import Data.Text


type PatternMatcher = RawAnalysisForm -> PatternMonad ()

data LogMessage
  = LogWarning String
  | LogError String
  deriving ( Show
           )

instance ToJSON LogMessage where
  toJSON (LogWarning msg) = String (pack msg)
  toJSON (LogError msg) = String (pack msg)

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
