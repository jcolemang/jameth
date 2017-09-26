
module JLLexingTypes where


import Text.ParserCombinators.Parsec


type JLTokenizeError = ParseError

data JLTree
  = JLVal JLConst
  | JLId String
  | JLSList [JLTree]
  deriving (Show)

data JLConst
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLVoid
  deriving (Show, Eq)
