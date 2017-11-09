
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

analysisRoutes :: [(ByteString, Handler b AnalysisService ())]
analysisRoutes = [ ( "/scheme/execute", method POST handleScheme )
                 , ( "/scheme/analysis", method POST handleStaticAnalysis )
                 , ( "/", serveFile "../analysis-ui/index.html" )
                 ]

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
