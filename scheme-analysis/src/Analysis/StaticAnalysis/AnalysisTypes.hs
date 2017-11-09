
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.StaticAnalysis.AnalysisTypes
  ( AnalysisState (..)
  , AnalysisMonad (..)
  , Report (..)
  )
where

import Analysis.StaticAnalysis.ReferenceTypes
import Analysis.StaticAnalysis.FormTypes
import Analysis.StaticAnalysis.PatternTypes

import Scheme.Types
import Control.Monad.State
import Control.Monad.Identity
import Data.Map as M
import Data.Set as S
import Data.Aeson

data Report
  = Report
  { errors :: [Error]
  , suggestions :: Log
  } deriving ( Show )

instance ToJSON Report where
  toJSON (Report { errors = errs
                 , suggestions = sgtns
                 } ) =
    object [ "errors" .= errs
           , "suggestions" .= sgtns
           ]

newtype AnalysisMonad a
  = AnalysisMonad
  { staticAnalysis :: StateT AnalysisState Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState AnalysisState )

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
