
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Handlers.HandleExecution
import Handlers.HandleStaticAnalysis
import ServerTypes
import ServerHelpers

import Snap.Snaplet
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Data.Text
import Data.ByteString
import Path

analysisRoutes :: Path x File -> [(ByteString, Handler b AnalysisService ())]
analysisRoutes p =
  let strPath = toFilePath p
  in [ ( "/scheme/execute", method POST handleScheme )
     , ( "/scheme/analysis", method POST handleStaticAnalysis )
     , ( "/", serveFile strPath )
     ]

analysisServiceInit :: Path x File -> SnapletInit a AnalysisService
analysisServiceInit p = makeSnaplet "analysis" "Scheme Analysis" Nothing $ do
  addRoutes $ analysisRoutes p
  return AnalysisService

config :: Config Snap a
config =
  setErrorLog (ConfigIoLog print) $
  setAccessLog (ConfigIoLog print) $
  setPort 8080 defaultConfig

apiRoutes :: [(Text, Handler b Api ())]
apiRoutes = [("status", method GET setResponseOk)]

runServer :: Path x File -> IO ()
runServer p =
  serveSnaplet config (analysisServiceInit p)
