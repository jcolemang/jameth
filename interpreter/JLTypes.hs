
module JLTypes where

import Data.Map
import Text.Parsec
import Control.Monad.Trans.State
import Control.Monad.Trans.Either


-- | Evaluation Data types

type Environment m = Map String (JLValue m)

data EvaluationState m = EvaluationState
  { _environment :: Environment m
  }


-- | Formal Syntax

data JLProgram m
  = JLProgram [JLForm m]
  deriving (Show)

data JLForm m
  = JLFormExp (JLExpression m)
  deriving (Show)

data JLValue m
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLProc (JLClosure m)
  | JLVoid
  deriving (Show, Eq)

data JLClosure m
  = JLClosure ([JLValue m] -> EitherT EvaluationError (StateT (EvaluationState m) m) (JLValue m))

instance Show (JLClosure m) where
  show _ = "<closure>"

instance Eq (JLClosure m) where
  _ == _ = False

data JLExpression m
  = JLValue  (JLValue m) SourcePos
  | JLVar    String SourcePos
  | JLQuote  String SourcePos
  | JLLambda SourcePos
  | JLTwoIf  (JLExpression m) (JLExpression m)   (JLExpression m) SourcePos
  | JLOneIf  (JLExpression m) (JLExpression m)   SourcePos
  | JLApp    (JLExpression m) [JLExpression m] SourcePos
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
  deriving (Show)
