
module Scheme.Forms
  (  annotation
  , form

  , isVar
  , isLambda
  , isApp

  , Closure (..)
  , Bodies
  , Arity (..)
  , Constant (..)
  , SourcePos (..)
  , LocalEnvironment
  , GlobalEnvironment
  , Form
  , Formals (..)
  , RawForm (..)
  , Program (..)
  , Annotated (..)
  , Annotation (..)
  , Label
  , Environment (..)
  , SList (..)
  , Tree (..)
  )
where

import Scheme.Environments

data Tree
  = TreeVal Constant SourcePos
  | TreeId String SourcePos
  | TreeSList [Tree] SourcePos
  deriving (Show)

data SourcePos
  = SP Int Int
  | PrimitiveSource
  | ExpandedSource
  deriving (Show, Eq)

data Constant
  = SStr  String
  | SBool Bool
  | SInt  Integer
  | SNum  Double
  | SSymbol String
  | SVoid
  deriving (Show, Eq)

newtype Program
  = Program [Form]
  deriving (Show)

type Label = Int

data Annotation
  = Ann
  { pos :: SourcePos
  , label :: Label
  , source :: Tree
  } deriving (Show)

type Form
  = Annotated Annotation RawForm

type Bodies = [Form]

data SList
  = Symbol String
  | Constant Constant
  | SList [SList]
  deriving ( Show )

data RawForm
  = Const Constant
  | Var String LexicalAddress
  | Quote SList
  | Lambda Formals Bodies
  | TwoIf Form Form Form
  | Define String Form
  | App Form [Form]
  | Set String LexicalAddress Form
  deriving ( Show )

isVar :: Form -> Bool
isVar (A _ (Var _ _)) = True
isVar _ = False

isLambda :: Form -> Bool
isLambda (A _ (Lambda _ _)) = True
isLambda _ = False

isApp :: Form -> Bool
isApp (A _ (App _ _)) = True
isApp _ = False

data Annotated ann f
  = A ann f
  deriving (Show)

instance Monoid Program where
  mempty = Program []
  mappend (Program fs) (Program fs') =
    Program (fs ++ fs')

data Formals
  = SymbolFormal    String
  | Formals         [String]
  | ImproperFormals String [String] String
  deriving ( Show )

data Arity
  = Exactly Int
  | AnyNum
  | AtLeast Int
  | Cases [Arity]

data Closure a
  = Closure Formals Bodies (LocalEnvironment a) SourcePos
  | Primitive String Arity

instance Eq (Closure a) where
  (Closure _ _ _ sp) == (Closure _ _ _ sp') = sp == sp'
  (Primitive name _) == (Primitive name' _) = name == name'
  _ == _ = False

instance Show (Closure a) where
  show _ = "<closure>"

annotation :: Annotated ann f -> ann
annotation (A a _) = a

form :: Annotated ann f -> f
form (A _ f) = f
