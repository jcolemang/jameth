
module Types where

data JLLiteral
  = JLStr String
  | JLBool Bool
  | JLInt Integer
  | JLNum Double
  deriving Show

data JLExpression
  = JLConstant JLLiteral
  | JLVariable String
  deriving Show
