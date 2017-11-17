
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.StaticAnalysis.Types
  ( module Analysis.StaticAnalysis.FormTypes
  , module Analysis.StaticAnalysis.PatternTypes
  , module Analysis.StaticAnalysis.AnalysisTypes
  , module Analysis.StaticAnalysis.ReferenceTypes
  , module Analysis.StaticAnalysis.ParseTypes
  , getTypes
  , runAnalysis
  , runAnalysisValue
  , runAnalysisState

  , addQuantsToRef
  , getQuantsFromRef
  , getQuantFromLabel

  , getQuantTypes
  , getAllQuantTypes
  , getRefTypes

  , addTypeToQuant

  , newLabeledQuant
  , newGlobalQuant

  )
where

import Analysis.StaticAnalysis.AnalysisTypes
import Analysis.StaticAnalysis.ReferenceTypes
import Analysis.StaticAnalysis.FormTypes
import Analysis.StaticAnalysis.ParseTypes
import Analysis.StaticAnalysis.PatternTypes
import Scheme.Types hiding ( Formals
                           , Closure
                           , Symbol
                           )

import Control.Monad.State
import Control.Monad.Identity
import Data.Map as M
import Data.Set as S

-- getId :: Ref -> Int
-- getId (Ref x) = x

getTypes :: AnalysisForm -> Set Type
getTypes (A ann _) = outTypes ann

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

newLabeledQuant :: Label -> AnalysisMonad Quant
newLabeledQuant lab = do
  labelMap <- lqMap <$> get
  case M.lookup lab labelMap of
    Nothing -> do
      q <- newQuant
      modify $ \s -> let newMap = M.insert lab q labelMap
                     in if newMap == labelMap
                        then s
                        else
                          s { lqMap = newMap
                            , modified = True
                            }

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
                 let newQuants = M.insert name q gqs
                 in s { globalQuants = newQuants }
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

getQuantFromLabel :: Label -> AnalysisMonad Quant
getQuantFromLabel lab = do
  labelMap <- lqMap <$> get
  case M.lookup lab labelMap of
    Nothing -> newLabeledQuant lab
    Just quant -> return quant

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
