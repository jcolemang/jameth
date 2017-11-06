
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Analysis.AbstractInterpreter.Types
  ( AbstractValue (..)
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
  , visit
  , numVisits
  , runtimeError
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

data AnalysisState
  = AnalysisState
  { inMap     :: Map Label [AbstractValue]
  , outMap    :: Map Label [([AbstractValue], AbstractValue)]
  , visits    :: Map Label Int
  , labelMap  :: Map Label (Form Annotation)
  , localEnv  :: LocalEnvironment AbstractValue
  , globalEnv :: GlobalEnvironment AbstractValue
  , modified  :: Bool
  }

data RuntimeError
  = BadNumArguments
  | NotAProcedure
  | UnboundVar String
  | TypeError [AbstractValue]
  | ValueError [AbstractValue]
  -- | Error RuntimeError
  deriving ( Show
           , Eq
           )

type AbstClosure = Closure Annotation AbstractValue

data AbstractValue
  = AbstList [AbstractValue]
  | AbstProc AbstClosure
  | AbstBool
  | AbstNum
  | AbstStr
  | AbstSym
  | Branch AbstractValue AbstractValue
  | AbstVoid
  | AbstUndefined
  | Top
  | Bottom
  deriving ( Show
           , Eq
           )

runtimeError :: RuntimeError -> SourcePos -> AnalysisMonad a
runtimeError err sp =
  throwError (RT err sp)

data AnalysisError
  = RT RuntimeError SourcePos
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

visit :: Label -> AnalysisMonad ()
visit lab = do
  s <- get
  let mvs = M.lookup lab (visits s)
  case mvs of
    Nothing ->
      error "There is an issue with the labels."
    Just vs ->
      let newVs =
            insert lab (vs + 1) (visits s)
      in put $ s { visits = newVs }

numVisits :: Label -> AnalysisMonad Int
numVisits lab = do
  vs <- M.lookup lab . visits <$> get
  case vs of
    Nothing ->
      error "Another issue with labels"
    Just v ->
      return v

createLabelMap :: Program Annotation -> Map Label (Form Annotation)
createLabelMap (Program fs) =
  let labels = snd (runWriter $ mapM getLabels fs)
  in fromList labels

getLabels :: Form Annotation -> Writer [(Label, Form Annotation)] ()
getLabels f =
  let lab = label $ annotation f
  in case form f of
    Lambda _ bodies -> do
      mapM_ getLabels bodies
      tell [(lab, f)]
    TwoIf test true false -> do
      getLabels test
      getLabels true
      getLabels false
    Define _ frm -> do
      getLabels frm
      tell [(lab, f)]
    App rator rands -> do
      tell [(lab, f)]
      getLabels rator
      mapM_ getLabels rands
    _ -> -- forms which have no inner labels
      tell [(lab, f)]

initialState :: Program Annotation -> AnalysisState
initialState prog =
  let labels = createLabelMap prog
      initMap = fromList $ fmap (\x -> (x, [])) (keys labels)
      numVs = fromList $ fmap (\x -> (x, 0)) (keys labels)
  in AnalysisState
     { outMap    = initMap
     , inMap     = initMap
     , visits    = numVs
     , labelMap  = createLabelMap prog
     , localEnv  = createEmptyEnv
     , globalEnv = defaultGlobalEnv
     , modified  = False
     }

putOutput :: Label
          -> [AbstractValue]
          -> AbstractValue
          -> AnalysisMonad AbstractValue
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
          -> [AbstractValue]
          -> AnalysisMonad (Maybe AbstractValue)
getOutput lab inps = do
  m <- outMap <$> get
  case M.lookup lab m of
    Nothing ->
      error $ "Something went wrong with the labels. Label " ++ show lab ++ " not found."
    Just vals ->
      return $ Prelude.lookup inps vals

defaultGlobalEnv :: GlobalEnvironment AbstractValue
defaultGlobalEnv =
  createGlobalEnv (fmap (second AbstProc) primitiveProcedures)

createBranch :: AbstractValue -> AbstractValue -> AbstractValue
createBranch a b =
  if a == b
  then a
  else case (a, b) of
         (Branch a' a'', Branch b' b'') ->
           if | a' == b' ->
                Branch a' (Branch a'' b'')
              | a' == b'' ->
                Branch a' (Branch a'' b')
              | a'' == b' ->
                Branch a' (Branch a'' b'')
              | a'' == b'' ->
                Branch a' (Branch a'' b')
              | True ->
                Branch (Branch a' a'') (Branch b' b'')
         (Branch a' a'', b') ->
           if | a' == b' ->
                Branch b' a''
              | a'' == b' ->
                Branch a' b'
              | True ->
                Branch (Branch a' a'') b'
         (a', Branch b' b'') ->
           if | a' == b' ->
                Branch b' b''
              | a' == b'' ->
                Branch b' b''
              | True ->
                Branch a' (Branch b' b'')
         (a', b') ->
           Branch a' b'
