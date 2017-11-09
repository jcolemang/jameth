
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Types where

import Scheme.Types
import Analysis.StaticAnalysis.Types

import Data.Aeson


data SchemeRequest
  = Scheme String

data SchemeRespone
  = Parsed (Program Annotation)

data AnalysisReport
  = AnalysisReport
  { errors :: [Error]
  , comments :: Log
  }

instance FromJSON SchemeRequest where
  parseJSON =
    withObject "" (\o ->
                        do code <- o .: "code"
                           return $ Scheme code)

instance ToJSON SchemeRespone where
  toJSON (Parsed t) =
    object [ "parsed" .= show t ]
