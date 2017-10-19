
module ServerHelpers where

-- import ServerTypes

import Snap.Snaplet
import Snap.Core
import Data.Aeson
import Data.Text

setResponseOk :: Handler b v ()
setResponseOk = modifyResponse $ setResponseCode 200

respondJSON :: Value -> Handler b v ()
respondJSON v =
  writeText (pack $ show v)
