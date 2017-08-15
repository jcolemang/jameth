
module JLParse where


import Data.Map
import Control.Monad.Trans.Except


data SyntacticValue
  = X

data SyntacticEnvironment
  = Environment (Map String SyntacticValue)

data SyntaxTree
  = Y
  deriving (Show)

data JLParseError
  = Z
  deriving (Show)


parseJL :: String -> ExceptT JLParseError m SyntaxTree
parseJL = undefined
