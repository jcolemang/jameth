
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Handlers.HandleScheme
import ServerTypes
import ServerHelpers

import Snap.Snaplet
import Snap.Core
import Snap.Http.Server
import Data.Text
import Data.ByteString

analysisRoutes :: [(ByteString, Handler b AnalysisService ())]
analysisRoutes = [("/", method POST handleScheme)]

analysisServiceInit :: SnapletInit a AnalysisService
analysisServiceInit = makeSnaplet "analysis" "Scheme Analysis" Nothing $ do
  addRoutes analysisRoutes
  return AnalysisService

config :: Config Snap a
config =
  setErrorLog (ConfigIoLog print) $
  setAccessLog (ConfigIoLog print) $
  setPort 8080 defaultConfig

apiRoutes :: [(Text, Handler b Api ())]
apiRoutes = [("status", method GET setResponseOk)]

runServer :: IO ()
runServer =
  serveSnaplet config analysisServiceInit
