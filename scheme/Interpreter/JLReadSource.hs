
module Interpreter.JLReadSource where

import Scheme.JLTypes
import Scheme.JLParsingTypes
import Scheme.JLParse

import Path

readSourceFile :: Path Rel File -> IO (Either JLParseError Program)
readSourceFile p = do
  contents <- readFile (toFilePath p)
  return $ runJLParse contents
