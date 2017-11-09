
module Handlers.HandleStaticAnalysis
  ( handleStaticAnalysis
  )
where

import Analysis.StaticAnalysis.Analysis
import ServerHelpers
import Handlers.Helpers
import Scheme.Parse

import Snap.Snaplet
import Snap.Core
import Handlers.Types
import ServerTypes
import Data.Aeson

import Debug.Trace

handleStaticAnalysis :: Handler b AnalysisService ()
handleStaticAnalysis = do
  mcode <- readRequestBody 100000
  case decode mcode of
    Nothing ->
      decodeFailure mcode
    Just (Scheme code) ->
      let eitherParse = runParse code
      in case eitherParse of
           Left parseError -> do
             setBadRequest
             respondJSON (toJSON parseError)
           Right prog ->
             let report = generateReport prog
             in do
               setResponseOk
               traceShowM report
               respondJSON (toJSON report)
