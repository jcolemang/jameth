
module JLSyntax where


import JLTypes
import Control.Monad (mapM)


syntaxExpand :: JLSyntax -> Either SyntaxError JLExpression
syntaxExpand (JLSConst x) =
  return $ JLConst x
syntaxExpand (JLSVar x) =
  return $ JLVar x
syntaxExpand (JLSApp sf sargs) = do
  f <- syntaxExpand sf
  args <- mapM syntaxExpand sargs
  return $ JLApp f args
