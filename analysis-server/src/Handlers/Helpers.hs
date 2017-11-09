
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Helpers where

import ServerTypes
import ServerHelpers

import Snap.Snaplet
import Data.ByteString.Lazy.Char8
import Data.Aeson


decodeFailure :: ByteString -> Handler b AnalysisService ()
decodeFailure code =
  let errorString = "Could not parse request.\n"
                    ++ "Must give input in format: "
                    ++ "{\"code\": \"<scheme-code>\"}"
  in
    respondJSON $ object [ "error"
                           .= (errorString :: String)
                         , "Received"
                           .= unpack code
                         ]
