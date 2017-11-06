
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.StaticAnalysis.Types
  ( AnalysisMonad
  , AnalysisState
  -- , Contour
  , Quant
  , Type (..)
  , Error (..)
  -- , Formals (..)

  , runAnalysis
  , runAnalysisValue
  , runAnalysisState

  -- , addQuantsToContour
  -- , addQuantsToLabel
  , addQuantsToRef
  , getQuantsFromRef

  -- , getQuantsAnn
  , getQuantTypes
  -- , getQuantsLabel
  -- , getContourTypes

  , addTypeToQuant
  , addTypesToQuant

  , newQuant
  -- , newContour
  -- , getContour
  )
where

import Scheme.Types hiding ( Formals )
import Scheme.PrimitiveProcedures
import Analysis.StaticAnalysis.AnalysisForms

import Control.Monad.State
import Control.Monad.Identity
import Data.Map as M
import Data.Set as S

import Debug.Trace

-- newtype Contour = Contour Int
-- deriving ( Eq
--          , Show
--          )

-- newContour :: AnalysisMonad Contour
-- newContour = do
--   c <- currContour <$> get
--   modify $ \s@AnalysisState { currContour = Contour i
--                             , valueTable  = vt
--                             } ->
--              s { currContour = Contour $ i + 1
--                , valueTable  = M.insert c S.empty vt
--                }
--   return c

-- getContour :: Label -> AnalysisMonad Contour
-- getContour lab = do
--   m <- labelContourMap <$> get
--   case M.lookup lab m of
--     Nothing -> do
--       traceShowM $ "Creating a new contour for label: " ++ show lab
--       c <- newContour
--       modify $ \s -> s { labelContourMap = M.insert lab c m }
--       getContour lab
--     Just c ->
--       return c

-- instance Ord Contour where
--   compare (Contour a) (Contour b) = compare a b

data Error
  = NotAProcedure
  deriving ( Show
           , Eq
           )

instance Ord Error where -- Needed for sets
  _ `compare` _ = EQ

-- data Formals
--   = Formals [Contour]
--   | Placeholder
--   deriving ( Show )

data Type
  = Top
  | Numeric
  | Void
  | StaticProc Ref AnalysisFormals
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

  StaticProc a _ `compare` StaticProc b _ = a `compare` b
  StaticProc {}  `compare` _              = GT
  _              `compare` StaticProc {}  = LT

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

newtype AnalysisMonad a
  = AnalysisMonad
  { staticAnalysis :: StateT AnalysisState Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState AnalysisState )

-- instance Environment AnalysisMonad Contour where
--   getLocalEnv = localEnv <$> get
--   getGlobalEnv = globalEnv <$> get
--   putLocalEnv env = modify $ \s -> s { localEnv = env }
--   putGlobalEnv env = modify $ \s -> s { globalEnv = env }

-- initialGlobal :: [(String, Ref)]
-- initialGlobal =
--   let names = fmap fst primitiveProcedures
--   in flip fmap (zip [0..] names) $ \(i, name) -> (name, Ref i)

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
  undefined
  -- snd $ runAnalysis prog am

runAnalysisValue :: AnalysisProgram -> AnalysisMonad a -> a
runAnalysisValue prog am =
  undefined
  -- fst $ runAnalysis prog am


-- | Quant Manipulation

newQuant :: Label -> AnalysisMonad Quant
newQuant lab = do
  m <- lqMap <$> get
  case M.lookup lab m of
    Nothing -> do
      qNum <- currQuantNum <$> get
      modify $ \s -> s { currQuantNum = qNum + 1 }
      return $ Quant qNum
    Just q ->
      return q
  -- st <- get
  -- let lcMap = labelContourMap st
  -- case M.lookup lab lcMap of
  --   Just cont -> do
  --     let cqMap = contourQuantMap st
  --     case M.lookup cont cqMap of
  --       Nothing -> do
  --         qCont <- newContour
  --         let q = Quant qCont
  --         modify $ \s@AnalysisState { typeTable = tt
  --                                   , valueTable = vtMap
  --                                   } ->
  --                    s { valueTable =
  --                          M.insert cont (S.singleton (Quant qCont)) vtMap
  --                      , typeTable =
  --                          M.insert q S.empty tt
  --                      , contourQuantMap =
  --                          M.insert cont q cqMap
  --                      }
  --         newQuant lab
  --       Just x ->
  --         return x
  --   Nothing -> do
  --     c <- newContour
  --     modify $ \s@AnalysisState { labelContourMap = m
  --                               } ->
  --                s { labelContourMap = M.insert lab c m
  --                  }
  --     newQuant lab

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
  -- vt <- valueTable <$> get
  -- case M.lookup c vt of
  --   Nothing ->
  --     modify $ \s@AnalysisState { valueTable = qs } ->
  --               s { valueTable = M.insert c qSet qs }
  --   Just quants ->
  --     modify $ \s@AnalysisState { valueTable = qs } ->
  --               s { valueTable = M.insert c (quants `S.union` qSet) qs }

-- addQuantsToLabel :: Label -> Set Quant -> AnalysisMonad ()
-- addQuantsToLabel l qSet = do
--   cm <- labelContourMap <$> get
--   case M.lookup l cm of
--     Nothing -> do
--       c <- newContour
--       modify $ \s -> s { labelContourMap = M.insert l c cm }
--       addQuantsToLabel l qSet
--     Just c ->
--       addQuantsToContour c qSet

-- addQuantsToAnn :: Annotation -> Set Quant -> AnalysisMonad ()
-- addQuantsToAnn ann =
--   addQuantsToLabel (label ann)

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

-- getQuantsLabel :: Label -> AnalysisMonad (Set Quant)
-- getQuantsLabel l = do
--   cm <- labelContourMap <$> get
--   case M.lookup l cm of
--     Nothing -> do
--       c <- newContour
--       modify $ \s ->
--                  s { labelContourMap = M.insert l c cm }
--       return S.empty
--     Just x ->
--       getQuantsFromContour x

getQuantsFromContour :: Ref -> AnalysisMonad (Set Quant)
getQuantsFromContour c = do
  undefined
  -- qs <- valueTable <$> get
  -- case M.lookup c qs of
  --   Nothing -> do
  --     modify $ \s ->
  --                s { valueTable = M.insert c S.empty qs }
  --     getQuantsFromContour c
  --   Just quants ->
  --     return quants

-- getQuantsAnn :: Annotation -> AnalysisMonad (Set Quant)
-- getQuantsAnn ann =
--   getQuantsLabel (label ann)

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
  quants <- getQuantsFromContour c
  S.unions <$> mapM getQuantTypes (S.toList quants)
