
module JLTypes where

import Text.Parsec


-- | Evaluation Data types

data EvaluationState = NothingYet


-- | Formal Syntax

data JLProgram
  = JLProgram [JLForm]
  deriving (Show)

data JLForm
  = JLFormExp JLExpression
  deriving (Show)

data JLValue
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLProc JLClosure
  | JLVoid
  deriving (Show, Eq)

data JLClosure
  = JLClosure
  deriving (Show, Eq)

data JLExpression
  = JLValue  JLValue SourcePos
  | JLVar    String SourcePos
  | JLQuote  String SourcePos
  | JLLambda SourcePos
  | JLTwoIf  JLExpression JLExpression   JLExpression SourcePos
  | JLOneIf  JLExpression JLExpression   SourcePos
  | JLApp    JLExpression [JLExpression] SourcePos
  deriving (Show)


-- | Evaluation Syntax


-- | Errors

data JamethError
  = ESyntax SyntaxError
  | EEval   EvaluationError


data SyntaxError
  = BadSyntax


data EvaluationError
  = JLEvalError
  | JLUndefined
