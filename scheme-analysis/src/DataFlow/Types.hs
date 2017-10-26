
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataFlow.Types
  ( AbstractValue
    ( AbstList
    , AbstProc
    , AbstBool
    , AbstNum
    , AbstStr
    , AbstNYI
    , AbstErr
    , Branch
    )
  , RuntimeError (..)
  , AnalysisState (..)
  , AnalysisMonad (..)
  , AbstClosure
  , AnalysisError (..)

  , createBranch
  , initialState
  , defaultGlobalEnv
  , putOutput
  , getOutput
  )
where

import Scheme.Types
import Scheme.PrimitiveProcedures

import Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Arrow

import Debug.Trace

data AnalysisState
  = AnalysisState
  { inMap     :: Map Label [AbstractValue]
  , outMap    :: Map Label [(AbstractValue, AbstractValue)]
  , labelMap  :: Map Label Form
  , localEnv  :: LocalEnvironment AbstractValue
  , globalEnv :: GlobalEnvironment AbstractValue
  , modified  :: Bool
  }

createLabelMap :: Program -> Map Label Form
createLabelMap (Program fs) =
  let labels = snd (runWriter $ mapM getLabels fs)
  in traceShow (fromList labels) (fromList labels)

getLabels :: Form -> Writer [(Label, Form)] ()
getLabels f =
  let lab = label $ annotation f
  in case form f of
    Lambda _ bodies -> do
      mapM_ getLabels bodies
      tell [(lab, f)]
    App rator rands -> do
      tell [(lab, f)]
      getLabels rator
      mapM_ getLabels rands
    TwoIf test true false -> do
      getLabels test
      getLabels true
      getLabels false
    _ ->
      tell [(lab, f)]

initialState :: Program -> AnalysisState
initialState prog =
  let labels = createLabelMap prog
      initMap = fromList $ fmap (\x -> (x, [])) (keys labels)
  in AnalysisState
     { outMap    = initMap
     , inMap     = initMap
     , labelMap  = createLabelMap prog
     , localEnv  = createEmptyEnv
     , globalEnv = defaultGlobalEnv
     , modified  = False
     }

putOutput :: Label -> AbstractValue -> AbstractValue -> AnalysisMonad AbstractValue
putOutput lab inp val = do
  s <- get
  let m = outMap s
  case M.lookup lab m of
    Nothing ->
      error $ "Label not in there: " ++ show lab ++ " " ++ show val
    Just outs ->
      let m' = insert lab ((inp, val) : outs) m
      in put ( s { outMap = m'
                 , modified = True
                 }
             ) >> return val

getOutput :: Label
          -> AbstractValue
          -> AbstractValue
          -> AnalysisMonad (Maybe AbstractValue)
getOutput lab inp val = do
  m <- outMap <$> get
  case M.lookup lab m of
    Nothing ->
      error "Something went wrong with the labels"
    Just vals ->
      return $ Prelude.lookup inp vals

defaultGlobalEnv :: GlobalEnvironment AbstractValue
defaultGlobalEnv =
  createGlobalEnv (fmap (second AbstProc) primitiveProcedures)

type AbstClosure = Closure AbstractValue

data AbstractValue
  = AbstList
  | AbstProc AbstClosure
  | AbstBool
  | AbstNum
  | AbstStr
  | AbstNYI
  | Branch AbstractValue AbstractValue
  | AbstErr RuntimeError
  deriving ( Show
           , Eq
           )

createBranch :: AbstractValue -> AbstractValue -> AbstractValue
createBranch a b =
  if a == b
  then a
  else Branch a b

data RuntimeError
  = BadNumArguments
  | NotAProcedure
  | UnboundVar
  | TypeError
  | Error RuntimeError
  deriving ( Show
           , Eq
           )

data AnalysisError
  = Placeholder
  deriving (Show)

newtype AnalysisMonad a
  = AnalysisMonad
  { runAnalysis :: ExceptT AnalysisError (StateT AnalysisState Identity) a
  } deriving ( Functor, Applicative, Monad,
               MonadState AnalysisState, MonadError AnalysisError )

instance Environment AnalysisMonad AbstractValue where
  getLocalEnv = localEnv <$> get
  getGlobalEnv = globalEnv <$> get
  putLocalEnv l = modify $ \s -> s { localEnv = l }
  putGlobalEnv g = modify $ \s -> s { globalEnv = g }
