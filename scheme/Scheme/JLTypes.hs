
module Scheme.JLTypes where

import Data.Map
import Text.Parsec (SourcePos)

data JLConstant
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLSymbol String
  | JLVoid
  deriving (Show, Eq)

data JLFormals
  = JLSymbolFormal    String
  | JLFormals         [String]
  | JLImproperFormals String [String] String
  deriving (Show)

data JLProgram
  = JLProgram [JLForm]
  deriving (Show)

data JLForm
  = JLValue JLValue JLSourcePos
  | JLVar String JLSourcePos
  | JLQuote JLValue JLSourcePos
  | JLLambda JLFormals [JLForm] JLSourcePos
  | JLLet [(String, JLForm)] [JLForm] JLSourcePos
  | JLTwoIf JLForm JLForm JLForm JLSourcePos
  | JLOneIf JLForm JLForm JLSourcePos
  | JLDefine String JLForm
  | JLApp JLForm [JLForm] JLSourcePos
  deriving (Show)

data JLClosure
  = JLClosure JLFormals [JLForm] (JLEnvironment JLValue) JLSourcePos
  | JLPrimitive

instance Show JLClosure where
  show _ = "<closure>"

data JLValue
  = JLConst JLConstant
  | JLProc JLClosure
  | JLList [JLValue]
  deriving (Show, Eq)

instance Eq JLClosure where
  _ == _ = False

data JLEnvironment a
  = JLEnv (Map String a) (JLEnvironment a)
  | JLEmptyEnv
  deriving (Show)

newtype GlobalEnvironment a = GlobalEnv (JLEnvironment a)
  deriving ( Show )

newtype LocalEnvironment a = LocalEnv (JLEnvironment a)
  deriving ( Show )

data JLSourcePos
  = SP SourcePos
  | Primitive
  deriving (Show, Eq)
