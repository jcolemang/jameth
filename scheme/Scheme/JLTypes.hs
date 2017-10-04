
module Scheme.JLTypes
  ( displayForm
  , getAddress
  , isValue, isVar, isQuote, isLambda, isLet, isTwoIf, isOneIf, isDefine, isApp
  , extendEnv, createEnv, putInEnv, createGlobalEnv, createEmptyEnv

  , Closure (..)
  , Arity (..)
  , Constant (..)
  , SourcePos (..)
  , LocalEnvironment
  , GlobalEnvironment
  , Form (..)
  , Value (..)
  , Program (..)

  -- Should be removed
  , globalReference
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

data JLFormals
  = JLSymbolFormal    String
  | JLFormals         [String]
  | JLImproperFormals String [String] String
  deriving (Show)

displayFormals :: JLFormals -> String
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

data Form
  = Value Value SourcePos
  | JLVar String LexicalAddress SourcePos
  | JLQuote Value SourcePos
  | JLLambda JLFormals [Form] SourcePos
  | JLLet [(String, Form)] [Form] SourcePos
  | JLTwoIf Form Form Form SourcePos
  | JLOneIf Form Form SourcePos
  | JLDefine String Form SourcePos
  | JLApp Form [Form] SourcePos
  deriving (Show)

isValue :: Form -> Bool
isValue Value {} = True
isValue _ = False

isVar :: Form -> Bool
isVar JLVar {} = True
isVar _ = False

isQuote :: Form -> Bool
isQuote JLQuote {} = True
isQuote _ = False

isLambda :: Form -> Bool
isLambda JLLambda {} = True
isLambda _ = False

isLet :: Form -> Bool
isLet JLLet {} = True
isLet _ = False

isTwoIf :: Form -> Bool
isTwoIf JLTwoIf {} = True
isTwoIf _ = False

isOneIf :: Form -> Bool
isOneIf JLOneIf {} = True
isOneIf _ = False

isDefine :: Form -> Bool
isDefine JLDefine {} = True
isDefine _ = False

isApp :: Form -> Bool
isApp JLApp {} = True
isApp _ = False

displayForm :: Form -> String
displayForm (Value val _) =
  displayValue val
displayForm (JLVar name _ _) =
  name
displayForm (JLQuote val _) =
  "(quote " ++ show val ++ ")"
displayForm (JLLambda formals bodies _) =
  let bs = unwords (map displayForm bodies)
  in "(lambda " ++ displayFormals formals ++ " " ++ bs ++ ")"
displayForm (JLApp f args _) =
  let as = unwords (map displayForm args)
  in "(" ++ displayForm f ++ " " ++ as ++ ")"
displayForm _ =
  undefined

data Arity
  = Exactly Int
  | AnyNum
  | AtLeast Int
  | Cases [Arity]

data Closure
  = Closure JLFormals [Form] (LocalEnvironment Value) SourcePos
  | Primitive String Arity

instance Show Closure where
  show _ = "<closure>"

data Value
  = JLConst Constant
  | JLProc Closure
  | JLList [Value]
  deriving (Show, Eq)

displayValue :: Value -> String
displayValue (JLConst c) = displayConstant c
displayValue _ = undefined

instance Eq Closure where
  _ == _ = False

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
