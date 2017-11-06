
module Scheme.Forms
  (  annotation
  , form

  , isVar
  , isLambda
  , isApp

  , Closure (..)
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
  deriving ( Show )

data SourcePos
  = SP Int Int
  | PrimitiveSource
  | ExpandedSource
  deriving ( Show
           , Eq
           )

data Constant
  = SStr  String
  | SBool Bool
  | SInt  Integer
  | SNum  Double
  | SSymbol String
  | SVoid
  deriving ( Show
           , Eq
           )

newtype Program ann
  = Program [Form ann]
  deriving ( Show )

type Label = Int

data Annotation
  = Ann
  { pos :: SourcePos
  , label :: Label
  , source :: Tree
  } deriving ( Show )

type Form a
  = Annotated a (RawForm a)

data SList
  = Symbol String
  | Constant Constant
  | SList [SList]
  deriving ( Show )

data RawForm a
  = Const Constant
  | Var String LexicalAddress
  | Quote SList
  | Lambda Formals [Form a]
  | TwoIf (Form a) (Form a) (Form a)
  | Define String (Form a)
  | App (Form a) [Form a]
  | Set String LexicalAddress (Form a)
  deriving ( Show )

isVar :: Form a -> Bool
isVar (A _ (Var _ _)) = True
isVar _ = False

isLambda :: Form a -> Bool
isLambda (A _ (Lambda _ _)) = True
isLambda _ = False

isApp :: Form a -> Bool
isApp (A _ (App _ _)) = True
isApp _ = False

data Annotated ann f
  = A ann f
  deriving (Show)

instance Monoid (Program a) where
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

data Closure ann a
  = Closure Formals [Form ann] (LocalEnvironment a) SourcePos
  | Primitive String Arity

instance Eq (Closure ann a) where
  (Closure _ _ _ sp) == (Closure _ _ _ sp') = sp == sp'
  (Primitive name _) == (Primitive name' _) = name == name'
  _ == _ = False

instance Show (Closure ann a) where
  show _ = "<closure>"

annotation :: Annotated ann f -> ann
annotation (A a _) = a

form :: Annotated ann f -> f
form (A _ f) = f
