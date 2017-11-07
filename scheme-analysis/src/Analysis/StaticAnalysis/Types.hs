
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.StaticAnalysis.Types
  ( AnalysisMonad
  , AnalysisState
  , Quant (..)
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

  , getTypes
  , runAnalysis
  , runAnalysisValue
  , runAnalysisState

  , addQuantsToRef
  , getQuantsFromRef

  , getQuantTypes
  , getAllQuantTypes
  , getRefTypes

  , addTypeToQuant
  , addTypesToQuant

  , newLabeledQuant
  , newQuant
  , newGlobalQuant
  -- , getGlobalQuant
  -- , setGlobal
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
  | WrongNumArgs
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
  | Boolean
  | List
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

  Boolean `compare` Boolean = EQ
  Boolean `compare` _       = GT
  _ `compare` Boolean       = LT

  List `compare` List = EQ
  List `compare` _       = GT
  _ `compare` List       = LT

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
  { analysisQuantNum :: Int
  , modified :: Bool
  -- | Variables' possible states
  , analysisValueTable :: Map Ref (Set Quant)
  -- | The possible type of each quant
  , analysisTypeTable :: Map Quant (Set Type)
  -- | Repeatedly creating the same quant for a given label
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

getTypes :: AnalysisForm -> Set Type
getTypes (A ann _) = outTypes ann

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
  { currIdentifier :: Int -- for refs
  , parseQuantNum   :: Int
  , localEnv       :: LocalEnvironment Ref
  , globalEnv      :: GlobalEnvironment Ref
  , parseRefs      :: Map Ref (Set Quant)
  , parseValues    :: Map Quant (Set Type)
  , globalQuants   :: Map String Quant
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

class Monad m => Quantable m where
  typeTable        :: m (Map Quant (Set Type))
  modifyTypeTable  :: (Map Quant (Set Type) -> Map Quant (Set Type)) -> m ()
  valueTable       :: m (Map Ref (Set Quant))
  modifyValueTable :: (Map Ref (Set Quant) -> Map Ref (Set Quant)) -> m ()
  newQuant         :: m Quant

instance Quantable AnalysisMonad where
  typeTable = analysisTypeTable <$> get
  modifyTypeTable f = modify $ \s@AnalysisState { analysisTypeTable = att } ->
                                 s { analysisTypeTable = f att }
  valueTable = analysisValueTable <$> get
  modifyValueTable f = modify $ \s@AnalysisState { analysisValueTable = avt } ->
                                  s { analysisValueTable = f avt }
  newQuant = do
    qNum <- analysisQuantNum <$> get
    modify $ \s -> s { analysisQuantNum = qNum + 1 }
    return $ Quant qNum

instance Quantable AnalysisParse where
  typeTable = parseValues <$> get
  modifyTypeTable f = modify $ \s@ParseState { parseValues = pv } ->
                                 s { parseValues = f pv }
  valueTable = parseRefs <$> get
  modifyValueTable f = modify $ \s@ParseState { parseRefs = prs } ->
                                  s { parseRefs = f prs }
  newQuant = do
    qNum <- parseQuantNum <$> get
    modify $ \s -> s { parseQuantNum = qNum + 1 }
    return $ Quant qNum

runAnalysis :: AnalysisProgram
            -> ParseState
            -> AnalysisMonad a
            -> (a, AnalysisState)
runAnalysis _ parseState am =
  let s = AnalysisState { modified           = False
                        , analysisQuantNum   = parseQuantNum parseState
                        , lqMap              = M.empty
                        , analysisValueTable = parseRefs parseState
                        , analysisTypeTable  = parseValues parseState
                        }
  in runIdentity (runStateT (staticAnalysis am) s)

runAnalysisState :: AnalysisProgram
                 -> ParseState
                 -> AnalysisMonad a
                 -> AnalysisState
runAnalysisState prog parseState am =
  snd $ runAnalysis prog parseState am

runAnalysisValue :: AnalysisProgram
                 -> ParseState
                 -> AnalysisMonad a
                 -> a
runAnalysisValue prog parseState am =
  fst $ runAnalysis prog parseState am

-- getGlobalQuant :: String -> AnalysisParse (Maybe Quant)
-- getGlobalQuant name =
--   undefined

newLabeledQuant :: Label -> AnalysisMonad Quant
newLabeledQuant lab = do
  labelMap <- lqMap <$> get
  case M.lookup lab labelMap of
    Nothing -> do
      q <- newQuant
      modify $ \s -> s { lqMap = M.insert lab q labelMap }
      newLabeledQuant lab
    Just q ->
      return q

newGlobalQuant :: String -> AnalysisParse Quant
newGlobalQuant name = do
  quantMap <- globalQuants <$> get
  case M.lookup name quantMap of
    Nothing -> do
      q <- newQuant
      modify $ \s@ParseState { globalQuants = gqs } ->
                 s { globalQuants = M.insert name q gqs }
      newGlobalQuant name
    Just q ->
      return q

getQuantsFromRef :: Quantable m => Ref -> m (Set Quant)
getQuantsFromRef ref = do
  vt <- valueTable
  case M.lookup ref vt of
    Nothing -> do
      addQuantsToRef ref S.empty
      getQuantsFromRef ref
    Just quants ->
      return quants

addQuantsToRef :: Quantable m => Ref -> Set Quant -> m ()
addQuantsToRef ref qSet =
  modifyValueTable $ \vt ->
                       case M.lookup ref vt of
                         Nothing ->
                           M.insert ref qSet vt
                         Just existingQs ->
                           M.insert ref (existingQs `S.union` qSet) vt

addTypeToQuant :: Quantable m => Type -> Quant -> m ()
addTypeToQuant t quant =
  modifyTypeTable $ \tt ->
                      case M.lookup quant tt of
                        Nothing ->
                          M.insert quant (S.singleton t) tt
                        Just types ->
                          M.insert quant (S.insert t types) tt

addTypesToQuant :: Quantable m => Set Type -> Quant -> m ()
addTypesToQuant = undefined

getQuantTypes :: Quantable m => Quant -> m (Set Type)
getQuantTypes q = do
  tTable <- typeTable
  case M.lookup q tTable of
    Nothing -> do
      modifyTypeTable $ \_ ->
                          M.insert q S.empty tTable
      return S.empty
    Just ts ->
      return ts

getAllQuantTypes :: Quantable m => Set Quant -> m (Set Type)
getAllQuantTypes qs = S.unions <$> mapM getQuantTypes (S.toList qs)

getRefTypes :: Quantable m => Ref -> m (Set Type)
getRefTypes c = do
  quants <- getQuantsFromRef c
  S.unions <$> mapM getQuantTypes (S.toList quants)
