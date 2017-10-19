
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Scheme.Types
  ( displayForm
  , getAddress
  , isValue
  , isVar
  , isQuote
  , isLambda
  , isLet
  , isTwoIf
  , isOneIf
  , isDefine
  , isApp
  , extendEnv
  , createEnv
  , putInEnv, createEmptyEnv
  , extendGlobalEnv
  , createGlobalEnv
  , putInGlobalEnv
  , createEmptyGlobalEnv
  , annotation, form
  , globalPairs
  , globalReference
  , isGlobal
  , getEnvValue
  , popEnv
  , withNewEnv

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
  , Environment (..)

  )
where

import Data.Map hiding (map, foldl, foldl')
-- import Data.List hiding (insert)

data Constant
  = SStr  String
  | SBool Bool
  | SInt  Integer
  | SNum  Double
  | SSymbol String
  | SVoid
  deriving (Show, Eq)

displayConstant :: Constant -> String
displayConstant (SStr s) = show s
displayConstant (SBool True) = "#t"
displayConstant (SBool False) = "#f"
displayConstant (SInt num) = show num
displayConstant (SNum num) = show num
displayConstant (SSymbol x) = x
displayConstant SVoid = "#<void>"

displayFormals :: Formals -> String
displayFormals (SymbolFormal x) =
  x
displayFormals (Formals ids) =
  "(" ++ unwords ids ++ ")"

data Program
  = Program [Form]
  deriving (Show)

instance Monoid Program where
  mempty = Program []
  mappend (Program fs) (Program fs') =
    Program (fs ++ fs')

newtype Depth = Depth Int
              deriving (Show)
newtype Index = Index Int
              deriving (Show)

data LexicalAddress
  = Global String
  | Bound Depth Index
  deriving ( Show )

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
  = Closure Formals Bodies (LocalEnvironment Value) SourcePos
  | Primitive String Arity

displayClosure :: Closure -> String
displayClosure Closure {} = "<closure>"
displayClosure Primitive {} = "<primitive>"

instance Eq Closure where
  (Closure _ _ _ sp) == (Closure _ _ _ sp') = sp == sp'
  (Primitive name _) == (Primitive name' _) = name == name'
  _ == _ = False

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
displayForm (A _ (Var name addr)) =
  "(" ++ name ++ ":" ++ show addr ++ ")"
displayForm (A _ (Quote val)) =
  "(quote " ++ show val ++ ")"
displayForm (A _ (Lambda formals bodies)) =
  let bs = unwords (map displayForm bodies)
  in "(lambda " ++ displayFormals formals ++ " " ++ bs ++ ")"
displayForm (A _ (App f args)) =
  let as = unwords (map displayForm args)
  in "(" ++ displayForm f ++ " " ++ as ++ ")"
displayForm (A _ (Define name f)) =
  "(define " ++ name ++ " " ++ displayForm f ++ ")\n"

instance Show Closure where
  show _ = "<closure>"

data Value
  = Const Constant
  | Proc Closure
  | VList [Value]
  | Undefined
  deriving ( Show, Eq )

displayValue :: Value -> String
displayValue (Const c) = displayConstant c
displayValue (Proc p) = displayClosure p
displayValue _ = undefined
-- displayValue (VList vs) = intercolate " "

-- instance Eq (Closure a) where
--   _ == _ = False

data LocalEnvironment a
  = Env [(String, a)] (LocalEnvironment a)
  | EmptyEnv
  deriving (Show)

class (Monad m) => Environment m a | m -> a where
  getLocalEnv  :: m (LocalEnvironment a)

  getGlobalEnv :: m (GlobalEnvironment a)

  putLocalEnv  :: LocalEnvironment a -> m ()

  putGlobalEnv :: GlobalEnvironment a -> m ()


putInEnv :: Environment m a
         => String
         -> a
         -> LexicalAddress
         -> m ()
putInEnv name datum (Bound (Depth 0) (Index i)) = do
  (Env ls parent) <- getLocalEnv
  putLocalEnv $ Env (take (i-1) ls ++ [(name, datum)] ++ drop (i+1) ls) parent
putInEnv name datum (Bound (Depth i) idx) =
  putInEnv name datum (Bound (Depth $ i - 1) idx)
putInEnv n datum (Global _) = do
  (GlobalEnv m) <- getGlobalEnv
  putGlobalEnv (GlobalEnv $ insert n datum m)

popEnv :: Environment m a => m ()
popEnv = do
  (Env _ p) <- getLocalEnv
  putLocalEnv p
  return ()

withNewEnv :: Environment m a
           => [(String, a)]
           -> LocalEnvironment a
           -> m a
           -> m a
withNewEnv ps env task = do
  curr <- getLocalEnv
  putLocalEnv env
  extendEnv ps
  result <- task
  putLocalEnv curr
  return result

extendEnv :: Environment m a
          => [(String, a)]
          -> m ()
extendEnv m = do
  l <- getLocalEnv
  putLocalEnv $ Env m l

-- extendEnv :: Environment m a
--           => [(String, a)]
--           -> m ()
-- extendEnv m = do
--   l <- getLocalEnv
--   putLocalEnv $ Env m l

extendGlobalEnv :: Environment m a
                => [(String, a)]
                -> m ()
extendGlobalEnv newVals = do
  (GlobalEnv m) <- getGlobalEnv
  putGlobalEnv (GlobalEnv $ foldl (flip $ uncurry insert) m newVals)

putInGlobalEnv :: Environment m a
               => String
               -> a
               -> m ()
putInGlobalEnv name val = do
  (GlobalEnv m) <- getGlobalEnv
  putGlobalEnv (GlobalEnv $ insert name val m)


createEnv :: [(String, a)] -> LocalEnvironment a
createEnv m = Env m EmptyEnv

createEmptyEnv :: LocalEnvironment a
createEmptyEnv = EmptyEnv

createGlobalEnv :: [(String, a)] -> GlobalEnvironment a
createGlobalEnv = GlobalEnv . fromList

createEmptyGlobalEnv :: GlobalEnvironment a
createEmptyGlobalEnv = GlobalEnv $ fromList []

globalPairs :: GlobalEnvironment a -> [(String, a)]
globalPairs (GlobalEnv m) = toList m

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

getEnvValue :: Environment m a
            => LexicalAddress
            -> m (Maybe a)
getEnvValue (Global s) = do
  (GlobalEnv m) <- getGlobalEnv
  case Data.Map.lookup s m of
    Nothing -> return Nothing
    Just val -> return $ Just val
getEnvValue (Bound (Depth 0) (Index i)) = do
  (Env ls _) <- getLocalEnv
  return . Just . snd $ ls !! i
getEnvValue (Bound (Depth d) idx) =
  getEnvValue $ Bound (Depth (d - 1)) idx

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
