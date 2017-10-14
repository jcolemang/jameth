
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Snap.Core
import Snap.Http.Server

-- config :: Snap ()
config :: Config Snap a
config =
  setErrorLog (ConfigIoLog print) $
  setAccessLog (ConfigIoLog print) $
  setPort 8080 defaultConfig

paths :: Snap ()
paths =
  ifTop (writeText "hello")

runServer :: IO ()
runServer =
  httpServe config paths
