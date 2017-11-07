
module CodeGeneration.RandomGeneration where

import Scheme.Types


getExpression :: Int -> [String] -> Tree
getExpression num available =
  case num of
    0 ->
      undefined
