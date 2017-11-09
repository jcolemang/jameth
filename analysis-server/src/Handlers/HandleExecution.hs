
{-# LANGUAGE OverloadedStrings #-}

module Handlers.HandleExecution
  ( handleScheme
  )
where

import Interpreter.Evaluate
import Scheme.Parse
import ServerTypes
import ServerHelpers
import Handlers.Types
import Handlers.Helpers

import Snap.Snaplet
import Snap.Core
import Data.Aeson
import Control.Monad.Except


handleScheme :: Handler b AnalysisService ()
handleScheme = do
  mcode <- readRequestBody 2048
  case decode mcode of
    Nothing ->
      decodeFailure mcode
    Just (Scheme code) ->
      case runParse code of
        Left err -> do
          setBadRequest
          respondJSON $ toJSON err
        Right prog -> do
          val <- liftIO $ execEval prog
          case val of
            Left evalErr -> do
              setBadRequest
              respondJSON $ toJSON evalErr
            Right v -> do
              setResponseOk
              respondJSON (toJSON $ object ["result" .= v])
