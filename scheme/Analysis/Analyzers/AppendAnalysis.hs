
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Analyzers.AppendAnalysis where

import Analysis.AnalysisTypes
import Scheme.JLTypes

import Control.Monad.Writer

import Debug.Trace

appendPattern :: Pattern
appendPattern =
  AppMatches (MustBe $ OriginalGlobal "append")
             [ AppMatches (MustBe $ OriginalGlobal "list")
                          [ Anything
                          ]
             , Anything
             ]

appendAnalysis :: MonadWriter [LogMessage] m => Form -> m BoundForm
appendAnalysis x@(App (Var "append" _ asp)
                      (App (Var "list" _ _)
                       [first] _:rest@_) sp) =
  let betterVersion =
        App (Var "cons" (globalReference "cons") asp) (first:rest) sp
      before = displayForm x
      after = displayForm betterVersion
      message = "Instead of `" ++ before ++
                "`, why not write `" ++ after ++ "`?"
  in traceShowM "~~~~~ Here I am ~~~~~"
     >> logWarning message sp
     >> return (BF x)
appendAnalysis v =
  logMessage ("Just a test: " ++ show v) (getSource v)
  >> return (BF v)
  -- return ()
