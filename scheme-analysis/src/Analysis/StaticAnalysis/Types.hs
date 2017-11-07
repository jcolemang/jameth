
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.StaticAnalysis.Types
  ( AnalysisMonad
  , AnalysisState
  , Quant
  , Type (..)
  , AnalysisProgram (..)
  , ParseState (..)
  , AnalysisParse (..)
  , Ref (..)
  , AnalysisFormals (..)
  , AnalysisAnnotation (..)
  , RawAnalysisForm (..)
  , AnalysisForm
  , Error (..)
  , StaticClosure (..)

  -- , getTypes

  , runAnalysis
  , runAnalysisValue
  , runAnalysisState

  , addQuantsToRef
  , getQuantsFromRef

  , getQuantTypes
  , getRefTypes

  , addTypeToQuant
  , addTypesToQuant

  , newQuant
  , scaryNewQuant
  , getId
  )
where

import Scheme.Types hiding ( Formals
                           , Closure
                           )

import Control.Monad.State
import Control.Monad.Identity
import Data.Map as M
import Data.Set as S

data Error
  = NotAProcedure
  | TypeError
  deriving ( Show
           , Eq
           )

instance Ord Error where -- Needed for sets
  _ `compare` _ = EQ

data StaticClosure
  = StaticProc Ref AnalysisFormals
  | StaticPrimitive String ([Set Type] -> Set Type)

instance Eq StaticClosure where
  StaticPrimitive name _ == StaticPrimitive name' _ = name == name'
  StaticProc ref _ == StaticProc ref' _ = ref == ref'
  _ == _ = False

instance Show StaticClosure where
  show (StaticProc _ _) = "#<closure>"
  show (StaticPrimitive _ _) = "#<closure>"

instance Ord StaticClosure where
  StaticPrimitive name _ `compare` StaticPrimitive name' _ =
    name `compare` name'
  StaticPrimitive _ _ `compare` _ = GT
  _ `compare` StaticPrimitive _ _ = LT

  StaticProc ref _ `compare` StaticProc ref' _ =
    ref `compare` ref'

data Type
  = Top
  | Numeric
  | Void
  | Closure StaticClosure
  | Error Error
  | Str
  | Bottom
  deriving ( Show )

instance Eq Type where
  Top     == Top      = True
  Bottom  == Bottom   = True
  Numeric == Numeric  = True
  Void    == Void     = True
  _       == _        = False

instance Ord Type where -- Needed for sets
  Top `compare` Top = EQ
  Top `compare` _   = GT
  _   `compare` Top = LT

  Numeric `compare` Numeric = EQ
  Numeric `compare` _       = GT
  _       `compare` Numeric = LT

  Void `compare` Void = EQ
  Void `compare` _    = GT
  _    `compare` Void = LT

  Closure a `compare` Closure b = a `compare` b
  Closure _ `compare` _ = GT
  _ `compare` Closure _ = LT

  Error a `compare` Error b = a `compare` b
  Error _ `compare` _       = GT
  _       `compare` Error _ = LT

  Str `compare` Str = EQ
  Str `compare` _   = GT
  _   `compare` Str = LT

  Bottom `compare` Bottom = EQ

newtype Quant
  = Quant
  {  qname :: Int
  } deriving ( Show )

instance Eq Quant where
  Quant { qname = a } == Quant { qname = b } = a == b

instance Ord Quant where
  Quant { qname = a } `compare` Quant { qname = b } = compare a b

data AnalysisState
  = AnalysisState
  { currQuantNum :: Int
  , modified :: Bool
  -- | Variables' possible states
  , valueTable :: Map Ref (Set Quant)
  -- | The possible type of each quant
  , typeTable :: Map Quant (Set Type)
  , lqMap :: Map Label Quant
  } deriving ( Show
             )

newtype Ref
  = Ref Int
  deriving ( Show )

getId :: Ref -> Int
getId (Ref x) = x

instance Eq Ref where
  Ref a == Ref b = a == b

instance Ord Ref where
  Ref a `compare` Ref b = compare a b

newtype AnalysisProgram
  = AnalysisProgram [AnalysisForm]
  deriving ( Show )

type AnalysisForm = Annotated AnalysisAnnotation RawAnalysisForm

data AnalysisAnnotation
  = AnalysisAnn
  { sourcePos :: SourcePos
  , label :: Label
  , outTypes :: Set Type
  }
  deriving ( Show )

-- getTypes :: AnalysisForm -> Set Type
-- getTypes (A ann _) = outTypes ann

data AnalysisFormals
  = AnalysisFormals [(String, Ref)]
  | SymbolFormal Ref
  deriving ( Show )

data RawAnalysisForm
  = AnalysisConst Constant
  | AnalysisVar String LexicalAddress Ref
  | AnalysisLambda Ref
                   AnalysisFormals
                   [AnalysisForm]
  | AnalysisDefine String Ref AnalysisForm
  | AnalysisApp Ref AnalysisForm [AnalysisForm]
  deriving ( Show )

data ParseState
  = ParseState
  { currIdentifier :: Int
  , localEnv :: LocalEnvironment Ref
  , globalEnv :: GlobalEnvironment Ref
  }

newtype AnalysisParse a
  = AnalysisParse
  { runAnalysisParse :: StateT ParseState Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState ParseState
             )

instance Environment AnalysisParse Ref where
  getLocalEnv = localEnv <$> get
  getGlobalEnv = globalEnv <$> get
  putLocalEnv env = modify $ \s -> s { localEnv = env }
  putGlobalEnv env = modify $ \s -> s { globalEnv = env }

newtype AnalysisMonad a
  = AnalysisMonad
  { staticAnalysis :: StateT AnalysisState Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState AnalysisState )

runAnalysis :: AnalysisProgram -> AnalysisMonad a -> (a, AnalysisState)
runAnalysis _ am =
  let s = AnalysisState { modified     = False
                        , currQuantNum = 0
                        , lqMap        = M.empty
                        , valueTable   = M.empty
                        , typeTable    = M.empty
                        }
  in runIdentity (runStateT (staticAnalysis am) s)

runAnalysisState :: AnalysisProgram -> AnalysisMonad a -> AnalysisState
runAnalysisState prog am =
  snd $ runAnalysis prog am

runAnalysisValue :: AnalysisProgram -> AnalysisMonad a -> a
runAnalysisValue prog am =
  fst $ runAnalysis prog am


-- | Quant Manipulation

newQuant :: Label -> AnalysisMonad Quant
newQuant lab = do
  m <- lqMap <$> get
  case M.lookup lab m of
    Nothing -> do
      qNum <- currQuantNum <$> get
      let newQ = Quant qNum
      modify $ \s -> s { currQuantNum = qNum + 1
                       , lqMap = M.insert lab newQ m }
      newQuant lab
    Just q ->
      return q

scaryNewQuant :: AnalysisMonad Quant
scaryNewQuant = do
  qNum <- currQuantNum <$> get
  modify $ \s -> s { currQuantNum = qNum + 1 }
  return $ Quant qNum

getQuantsFromRef :: Ref -> AnalysisMonad (Set Quant)
getQuantsFromRef ref = do
  vt <- valueTable <$> get
  case M.lookup ref vt of
    Nothing -> do
      addQuantsToRef ref S.empty
      getQuantsFromRef ref
    Just quants ->
      return quants

addQuantsToRef :: Ref -> Set Quant -> AnalysisMonad ()
addQuantsToRef ref qSet =
  modify $ \s@AnalysisState { valueTable = vt } ->
             case M.lookup ref vt of
               Nothing ->
                 s { valueTable = M.insert ref qSet vt }
               Just existingQs ->
                 let newQs =
                       existingQs `S.union` qSet
                 in
                   s { valueTable = M.insert ref newQs vt }

addTypeToQuant :: Type -> Quant -> AnalysisMonad ()
addTypeToQuant t quant =
  modify $ \s@AnalysisState { typeTable = tt } ->
             case M.lookup quant tt of
               Nothing ->
                 s { typeTable = M.insert quant (S.singleton t) tt }
               Just types ->
                 s { typeTable = M.insert quant (S.insert t types) tt }

addTypesToQuant :: Set Type -> Quant -> AnalysisMonad ()
addTypesToQuant = undefined

getQuantTypes :: Quant -> AnalysisMonad (Set Type)
getQuantTypes q = do
  tTable <- typeTable <$> get
  case M.lookup q tTable of
    Nothing -> do
      modify $ \s ->
                 s { typeTable = M.insert q S.empty tTable  }
      return S.empty
    Just ts ->
      return ts

getRefTypes :: Ref -> AnalysisMonad (Set Type)
getRefTypes c = do
  quants <- getQuantsFromRef c
  S.unions <$> mapM getQuantTypes (S.toList quants)
