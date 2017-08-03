
module JLTypes where

data JLLiteral
  = JLStr String
  | JLBool Bool
  | JLInt Integer
  | JLNum Double
  deriving (Show, Eq)

data JLExpression
  = JLConstant JLLiteral
  | JLVariable String
  deriving (Show, Eq)
