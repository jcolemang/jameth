
module Scheme.ReadSource where

import Scheme.Types
import Scheme.JLParsingTypes
import Scheme.Parse

import Path

readSourceFile :: Path Rel File -> IO (Either JLParseError Program)
readSourceFile p = do
  contents <- readFile (toFilePath p)
  return $ runParse contents
