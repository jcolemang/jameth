
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analysis.StaticAnalysis.ParseTypes
  ( ParseState (..)
  , AnalysisParse (..)
  )
where

import Scheme.Types
import Analysis.StaticAnalysis.FormTypes
import Analysis.StaticAnalysis.ReferenceTypes

import Data.Map as M
import Data.Set as S
import Control.Monad.State
import Control.Monad.Identity

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
