
module JLEval where

import JLTypes


-- | Evaluating Expressions

evalExpression :: JLExpression -> Either EvaluationError JLLiteral
evalExpression (JLConst lit) =
  return lit
evalExpression (JLVar _) =
  Left JLUndefined
evalExpression (JLApp _ _) =
 Left JLUndefined
