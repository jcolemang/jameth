
module JLTypes where

import Data.Map
import Text.Parsec
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


-- | Evaluation Data types

type Evaluation = StateT EvaluationState (ExceptT EvaluationError IO) JLValue

data Environment
  = GlobalEnv (Map String JLValue)
  | LocalEnv (Map String JLValue) Environment

data EvaluationState = EvaluationState
  { _environment :: Environment
  }


-- | Formal Syntax

data JLProgram
  = JLProgram [JLForm]
  deriving (Show)

data JLForm
  = JLFormExp JLExpression
  | JLFormDef JLDefinition
  deriving (Show)

data JLDefinition =
  JLVarDef JLVariableDefinition
  deriving (Show)

data JLVariableDefinition
  = JLDefine String JLExpression
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
  = JLClosure JLFormals JLBody SourcePos
  | JLPrimitive ([JLValue] -> Evaluation)

instance Show JLClosure where
  show _ = "<closure>"

instance Eq JLClosure where
  _ == _ = False

data JLExpression
  = JLValue  JLValue      SourcePos
  | JLVar    String       SourcePos
  | JLQuote  String       SourcePos
  | JLLambda JLFormals    JLBody         SourcePos
  | JLTwoIf  JLExpression JLExpression   JLExpression SourcePos
  | JLOneIf  JLExpression JLExpression   SourcePos
  | JLApp    JLExpression [JLExpression] SourcePos
  deriving (Show)

data JLFormals
  = JLSymbolFormal    String
  | JLFormals         [String]
  | JLImproperFormals (String, [String], String)
  deriving (Show)

data JLBody
  = JLBody [JLDefinition] (JLExpression, [JLExpression])
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
  | JLNotAProcedure
  deriving (Show)
