
module Scheme.JLTypes where

import Data.Map hiding (map)
import Text.Parsec (SourcePos)

data JLConstant
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLSymbol String
  | JLVoid
  deriving (Show, Eq)

displayConstant :: JLConstant -> String
displayConstant (JLStr s) = show s
displayConstant (JLBool True) = "#t"
displayConstant (JLBool False) = "#f"
displayConstant (JLInt num) = show num
displayConstant (JLNum num) = show num
displayConstant (JLSymbol x) = x
displayConstant JLVoid = "#<void>"

data JLFormals
  = JLSymbolFormal    String
  | JLFormals         [String]
  | JLImproperFormals String [String] String
  deriving (Show)

displayFormals :: JLFormals -> String
displayFormals formals =
  undefined

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
  | JLDefine String JLForm JLSourcePos
  | JLApp JLForm [JLForm] JLSourcePos
  deriving (Show)

displayForm :: JLForm -> String
displayForm (JLValue val _) =
  displayValue val
displayForm (JLVar name _) =
  name
displayForm (JLQuote val _) =
  "(quote " ++ show val ++ ")"
displayForm (JLLambda formals bodies _) =
  let bs = unwords (map displayForm bodies)
  in "(lambda " ++ displayFormals formals ++ " " ++ bs ++ ")"
displayForm (JLLet asgns bodies _) =
  undefined
displayForm (JLTwoIf cond thn els _) =
  undefined
displayForm (JLOneIf cond thn _) =
  undefined
displayForm (JLDefine def body _) =
  undefined
displayForm (JLApp f args _) =
  let as = unwords (map displayForm args)
  in "(" ++ displayForm f ++ " " ++ as ++ ")"

data Arity
  = Exactly Int
  | AnyNum
  | AtLeast Int
  | Cases [Arity]

data JLClosure
  = JLClosure JLFormals [JLForm] (JLEnvironment JLValue) JLSourcePos
  | JLPrimitive String Arity

instance Show JLClosure where
  show _ = "<closure>"

data JLValue
  = JLConst JLConstant
  | JLProc JLClosure
  | JLList [JLValue]
  deriving (Show, Eq)

displayValue :: JLValue -> String
displayValue (JLConst c) = displayConstant c
displayValue _ = undefined

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
