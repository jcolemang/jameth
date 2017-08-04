
module JLTypes where


-- | Formal Syntax

data JLProgram
  = JLProgram [JLForm]
  deriving (Show)

data JLForm
  = JLFormExp JLExpression
  deriving (Show)

data JLLiteral
  = JLStr String
  | JLBool Bool
  | JLInt Integer
  | JLNum Double
  deriving (Show, Eq)

data JLExpression
  = JLConst JLLiteral
  | JLVar String
  | JLQuote String
  | JLLambda
  | JLTwoIf JLExpression JLExpression JLExpression
  | JLOneIf JLExpression JLExpression
  | JLApp JLExpression [JLExpression]
  deriving (Show)


-- | Evaluation Syntax


-- | Errors

data JamethError
  = ESyntax SyntaxError
  | EEval EvaluationError


data SyntaxError
  = BadSyntax


data EvaluationError
  = JLEvalError
  | JLUndefined
