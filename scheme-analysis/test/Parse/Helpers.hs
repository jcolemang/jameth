
module Parse.Helpers where

import Scheme.Types

getFirstFormA (Right (Program (x:_))) = Just x
getFirstFormA _ = Nothing

getFirstForm (Right (Program (x:_))) = Just $ form x
getFirstForm _ = Nothing

getQuoted (Quote x) = Just x
getQuoted _ = Nothing

getApp x@(App _ _) = Just x
getApp _ = Nothing
