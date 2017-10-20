
module Scheme.ReadSource where

import Scheme.Types
import Scheme.ParseTypes
import Scheme.Parse

import Path

readSourceFile :: Path Rel File -> IO (Either ParseError Program)
readSourceFile p = do
  contents <- readFile (toFilePath p)
  return $ runParse contents
