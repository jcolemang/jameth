
module ServerHelpers where

-- import ServerTypes

import Snap.Snaplet
import Snap.Core
import Data.Aeson

setResponseOk :: Handler b v ()
setResponseOk = modifyResponse $ setResponseCode 200

setBadRequest :: Handler b v ()
setBadRequest = modifyResponse $ setResponseCode 400

respondJSON :: Value -> Handler b v ()
respondJSON v =
  writeLBS (encode v)
