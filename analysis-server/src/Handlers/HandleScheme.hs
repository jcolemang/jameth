
{-# LANGUAGE OverloadedStrings #-}

module Handlers.HandleScheme where

import Scheme.Parse
import Scheme.Types
import ServerTypes
import ServerHelpers

import Data.Aeson
import Snap.Snaplet
import Snap.Core
import Data.ByteString.Lazy.Char8

data SchemeRequest
  = Scheme String

data SchemeRespone
  = Parsed (Program Annotation)

instance FromJSON SchemeRequest where
  parseJSON =
    withObject "" (\o ->
                        do code <- o .: "code"
                           return $ Scheme code)

instance ToJSON SchemeRespone where
  toJSON (Parsed t) =
    object [ "parsed" .= show t ]


handleScheme :: Handler b AnalysisService ()
handleScheme = do
  mcode <- readRequestBody 2048
  case decode mcode of
    Nothing ->
      respondJSON $ object [ "error"
                             .= ("I don't know what this means" :: String)
                           , "Received"
                             .= unpack mcode
                           ]
    Just (Scheme code) ->
      case runParse code of
        Right prog -> do
          setResponseOk
          respondJSON $ object ["code" .= show prog]
        Left err ->
          respondJSON $ object ["error" .= show err]
