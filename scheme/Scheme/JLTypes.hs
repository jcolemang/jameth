
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scheme.JLTypes where

import Data.Map
import Text.Parsec
import Control.Monad.State
import Control.Monad.Except
import Control.Lens


-- | Lexing sort of things

data JLTokenizeError

data JLTree
  = JLVal JLConstant JLSourcePos
  | JLId String JLSourcePos
  | JLSList [JLTree] JLSourcePos
  deriving (Show)

data JLConstant
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLVoid
  deriving (Show, Eq)

-- | Evaluation Data types

newtype EvaluationMonad a
  = EvaluationMonad
  { runEvaluation :: StateT EvaluationState (ExceptT EvaluationError IO) a
  } deriving ( Functor, Applicative, Monad,
               MonadState EvaluationState, MonadError EvaluationError )

data BoundValue
  = BVal JLValue
  | BSyntax JLSyntax
  | EmptySlot
  deriving ( Show )

data JLEnvironment
  = JLEnvironment (Map String BoundValue) JLEnvironment
  | JLEmptyEnvironment
  deriving (Show)

newtype GlobalEnvironment = GlobalEnv JLEnvironment
  deriving ( Show )
newtype LocalEnvironment = LocalEnv JLEnvironment
  deriving ( Show )

newtype ParseMonad a
  = ParseMonad
  { runParser :: ExceptT JLParseError (StateT ParseState Identity) a
  } deriving ( Functor, Applicative, Monad,
               MonadState ParseState, MonadError JLParseError )

data ParseState
  = ParseState
  { localEnv :: LocalEnvironment
  , globalEnv :: GlobalEnvironment
  } deriving (Show)

data EvaluationState
  = EvaluationState
  { evalEnv :: JLEnvironment
  }

data JLSourcePos
  = SP SourcePos
  | Primitive
  deriving (Show, Eq)

data JLParseError
  = JLParseError JLSourcePos
  | JLUndefinedVariable String JLSourcePos
  | JLInvalidSyntax JLSourcePos
  deriving (Show)

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
  = JLDefine String JLForm
  deriving (Show)

data JLSyntax
  = BuiltIn String (JLTree -> ParseMonad JLForm)

instance Show JLSyntax where
  show (BuiltIn n _) = "#< " ++ n ++ " >"

data JLValue
  = JLConst JLConstant
  | JLProc JLClosure
  | JLList [JLValue]
  deriving (Show, Eq)

data JLClosure
  = JLClosure JLFormals JLBody JLEnvironment JLSourcePos
  | JLPrimitive ([JLValue] -> EvaluationMonad JLValue)

instance Show JLClosure where
  show _ = "<closure>"

instance Eq JLClosure where
  _ == _ = False

data JLExpression
  = JLValue  JLValue      JLSourcePos
  | JLVar    String       JLSourcePos
  | JLQuote  String       JLSourcePos
  | JLLambda JLFormals    JLBody         JLSourcePos
  | JLTwoIf  JLExpression JLExpression   JLExpression JLSourcePos
  | JLOneIf  JLExpression JLExpression   JLSourcePos
  | JLApp    JLForm [JLForm] JLSourcePos
  deriving (Show)

data JLFormals
  = JLSymbolFormal    String
  | JLFormals         [String]
  | JLImproperFormals String [String] String
  deriving (Show)

data JLBody
  = JLBody [JLDefinition] (JLExpression, [JLExpression])
  deriving (Show)


-- | Errors

data EvaluationError
  = JLEvalError         JLSourcePos
  | JLUndefined         JLSourcePos
  | JLTypeError         JLSourcePos
  | JLNotAProcedure     JLSourcePos
  | JLBadNumArgs        JLSourcePos JLSourcePos
  deriving (Show)

makeLenses ''EvaluationState
