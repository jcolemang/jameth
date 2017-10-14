
module Scheme.JLTypes
  ( displayForm
  , getAddress, getValue
  , isValue, isVar, isQuote, isLambda, isLet, isTwoIf, isOneIf, isDefine, isApp
  , extendEnv, createEnv, putInEnv, createGlobalEnv, createEmptyEnv
  , annotation, form

  , Closure (..)
  , Arity (..)
  , Constant (..)
  , SourcePos (..)
  , LocalEnvironment
  , GlobalEnvironment
  , Form
  , Formals (..)
  , RawForm (..)
  , Value (..)
  , Program (..)
  , Annotated (..)
  , Annotation (..)
  , Label

  -- Should be removed
  , globalReference
  , isGlobal
  )
where

import Data.Map hiding (map)

data Constant
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLSymbol String
  | JLVoid
  deriving (Show, Eq)

displayConstant :: Constant -> String
displayConstant (JLStr s) = show s
displayConstant (JLBool True) = "#t"
displayConstant (JLBool False) = "#f"
displayConstant (JLInt num) = show num
displayConstant (JLNum num) = show num
displayConstant (JLSymbol x) = x
displayConstant JLVoid = "#<void>"

displayFormals :: Formals -> String
displayFormals _ =
  undefined

data Program
  = Program [Form]
  deriving (Show)

newtype Depth = Depth Int
              deriving (Show)
newtype Index = Index Int
              deriving (Show)

data LexicalAddress
  = Global String
  | Bound Depth Index
  deriving ( Show )

-- TODO This should not be here
globalReference :: String -> LexicalAddress
globalReference = Global

isGlobal :: LexicalAddress -> Bool
isGlobal (Global _) = True
isGlobal _ = False

data Annotated ann f
  = A ann f
  deriving (Show)

annotation :: Annotated ann f -> ann
annotation (A a _) = a

form :: Annotated ann f -> f
form (A _ f) = f

type Label = Int

data Annotation
  = Ann
  { pos :: SourcePos
  , label :: Label
  } deriving (Show)

-- primitiveAnnotation =
--   Ann
--   { pos = PrimitiveSource
--   ,
--   }

type Form
  = Annotated Annotation RawForm

type Bodies = [Form]

data RawForm
  = Value Value
  | Var String LexicalAddress
  | Quote Value
  | Lambda Formals Bodies
  | Let [(String, Form)] Bodies
  | TwoIf Form Form Form
  | OneIf Form Form
  | Define String Form
  | App Form [Form]
  deriving (Show)

data Formals
  = SymbolFormal    String
  | Formals         [String]
  | ImproperFormals String [String] String
  deriving (Show)

data Arity
  = Exactly Int
  | AnyNum
  | AtLeast Int
  | Cases [Arity]

data Closure
  = Closure Formals Bodies (LocalEnvironment Form) SourcePos
  | Primitive String Arity

-- data ExpandedForm
--   = EValue Value
--   | EVar String LexicalAddress
--   | EQuote Value
--   | ELambda

isValue :: Form -> Bool
isValue (A _ Value {}) = True
isValue _ = False

isVar :: Form -> Bool
isVar (A _ Var {}) = True
isVar _ = False

isQuote :: Form -> Bool
isQuote (A _ Quote {}) = True
isQuote _ = False

isLambda :: Form -> Bool
isLambda (A _ Lambda {}) = True
isLambda _ = False

isLet :: Form -> Bool
isLet (A _ Let {}) = True
isLet _ = False

isTwoIf :: Form -> Bool
isTwoIf (A _ TwoIf {}) = True
isTwoIf _ = False

isOneIf :: Form -> Bool
isOneIf (A _ OneIf {}) = True
isOneIf _ = False

isDefine :: Form -> Bool
isDefine (A _ Define {}) = True
isDefine _ = False

isApp :: Form -> Bool
isApp (A _ App {}) = True
isApp _ = False

displayForm :: Form -> String
displayForm (A _ (Value val)) =
  displayValue val
displayForm (A _ (Var name _)) =
  name
displayForm (A _ (Quote val)) =
  "(quote " ++ show val ++ ")"
displayForm (A _ (Lambda formals bodies)) =
  let bs = unwords (map displayForm bodies)
  in "(lambda " ++ displayFormals formals ++ " " ++ bs ++ ")"
displayForm (A _ (App f args)) =
  let as = unwords (map displayForm args)
  in "(" ++ displayForm f ++ " " ++ as ++ ")"
displayForm _ =
  undefined

instance Show Closure where
  show _ = "<closure>"

data Value
  = JLConst Constant
  | JLProc Closure
  | JLList [Value]
  deriving (Show)

displayValue :: Value -> String
displayValue (JLConst c) = displayConstant c
displayValue _ = undefined

-- instance Eq (Closure a) where
--   _ == _ = False

data LocalEnvironment a
  = Env [(String, a)] (LocalEnvironment a)
  | EmptyEnv
  deriving (Show)

putInEnv :: String
         -> a
         -> LexicalAddress
         -> LocalEnvironment a
         -> GlobalEnvironment a
         -> (LocalEnvironment a, GlobalEnvironment a)
putInEnv name datum (Bound (Depth 0) (Index i)) (Env ls parent) g =
  ( Env (take (i-1) ls ++ [(name, datum)] ++ drop (i+1) ls) parent, g)
putInEnv name datum (Bound (Depth i) idx) (Env ls parent) g =
  let (nextLocal, nextGlobal) =
        putInEnv name datum (Bound (Depth $ i - 1) idx) parent g
  in (Env ls nextLocal, nextGlobal)
putInEnv n datum (Global _) env (GlobalEnv m) =
  (env, GlobalEnv $ insert n datum m)
putInEnv _ _ _ _ _ =
  error "A lexical addressing error occurred. Please report this as a bug."

extendEnv :: [(String, a)]
                  -> LocalEnvironment a
                  -> LocalEnvironment a
extendEnv = Env

createEnv :: [(String, a)] -> LocalEnvironment a
createEnv = flip extendEnv EmptyEnv

createEmptyEnv :: LocalEnvironment a
createEmptyEnv = EmptyEnv

createGlobalEnv :: [(String, a)] -> GlobalEnvironment a
createGlobalEnv = GlobalEnv . fromList

getAddress :: String
           -> LocalEnvironment a
           -> GlobalEnvironment a
           -> Maybe (a, LexicalAddress)
getAddress iden local (GlobalEnv global) =
  case findIdentifier iden 0 local of
    Nothing ->
      case Data.Map.lookup iden global of
        Nothing -> Nothing
        Just bv -> Just (bv, Global iden)
    v@(Just _) -> v

getValue :: LexicalAddress
         -> LocalEnvironment a
         -> GlobalEnvironment a
         -> Maybe a
getValue _ _ _ = undefined

findIdentifier :: String
               -> Int
               -> LocalEnvironment a
               -> Maybe (a, LexicalAddress)
findIdentifier iden d (Env ls parent) =
  case indexAndName iden 0 ls of
    Just (idx, val) -> Just (val, Bound (Depth d) (Index idx))
    Nothing -> findIdentifier iden (d+1) parent
findIdentifier _ _ EmptyEnv =
  Nothing

indexAndName :: Eq a => a -> Int -> [(a, b)] -> Maybe (Int, b)
indexAndName _ _ [] = Nothing
indexAndName name i ((x, val):rest) =
  if name == x
  then Just (i, val)
  else indexAndName name (i+1) rest


newtype GlobalEnvironment a = GlobalEnv (Map String a)
  deriving ( Show )

data SourcePos
  = SP Int Int
  | PrimitiveSource
  deriving (Show, Eq)
