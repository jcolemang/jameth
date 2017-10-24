
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataFlow.DataFlow where

import Scheme.Types
import Scheme.PrimitiveProcedures

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.List
import Control.Monad.Identity
import Data.Map
import Control.Monad.Writer
import Debug.Trace


-- initialLabel :: Form -> Label
-- initialLabel (A ann Value{}) = label ann
-- initialLabel (A ann Var{}) = label ann
-- initialLabel (A ann Quote{}) = label ann
-- initialLabel (A ann Lambda{}) = label ann
-- initialLabel (A ann Let{}) = label ann
-- initialLabel (A ann TwoIf{}) = label ann
-- initialLabel (A ann OneIf{}) = label ann
-- initialLabel (A ann Define{}) = label ann
-- initialLabel (A ann App{}) = label ann

finalLabel :: Form -> Int
finalLabel = undefined

newtype AnalysisMonad a
  = AnalysisMonad
  { runAnalysis :: ExceptT AnalysisError (StateT AnalysisState Identity) a
  } deriving ( Functor, Applicative, Monad,
               MonadState AnalysisState, MonadError AnalysisError )

instance Environment AnalysisMonad (Annotated Annotation RawForm) where
  getLocalEnv = localEnv <$> get
  getGlobalEnv = globalEnv <$> get
  putLocalEnv l = modify $ \s -> s { localEnv = l }
  putGlobalEnv g = modify $ \s -> s { globalEnv = g }

data AbstClosure
  = AbstClosure Formals Bodies (LocalEnvironment AbstractValue)

data AbstractValue
  = AbstList
  | AbstProc AbstClosure
  | AbstBool
  | AbstNum
  | AbstStr
  | AbstNYI


allPossibleValues :: Program -> AnalysisMonad a
allPossibleValues prog =
  undefined

data AnalysisState
  = AnalysisState
  { labelMap :: Map Label Form
  , localEnv :: LocalEnvironment Form
  , globalEnv :: GlobalEnvironment Form
  }

allPossible :: Form -> AnalysisMonad [AbstractValue]
allPossible (A _ (Const (SStr _))) =
  return [ AbstStr ]
allPossible (A _ (Const (SBool _))) =
  return [ AbstBool ]
allPossible (A _ (Const (SInt _))) =
  return [ AbstNum ]
allPossible (A _ (Const (SNum _))) =
  return [ AbstNum ]
allPossible (A _ (Const _)) =
  return [ AbstNYI ]
allPossible (A _ (TwoIf _ true false)) = do
  trueBranch <- allPossible true
  falseBranch <- allPossible false
  return $ trueBranch ++ falseBranch
allPossiblle (A _ (Lambda fs bods)) = do
  undefined
  -- env <- getLocalEnv
  -- return (AbstProc $ Closure fs env



-- runDataFlow :: Form -> [Either AnalysisError Program]
-- runDataFlow f =
--   undefined
  -- let initGlobal = initialState f
  --     exec m = fst $ runIdentity (runStateT (runExceptT (runAnalysis m))
  --                                 initGlobal)
  -- in fmap exec (evaluate f)

-- runAnalysis :: AnalysisMonad a -> Either AnalysisError (s, a)
-- runAnalysis am =
--   runIdentity (runStateT)

initialState :: Form -> AnalysisState
initialState f =
  AnalysisState
  { labelMap = createLabelMap f
  , localEnv = createEmptyEnv
  , globalEnv = createEmptyGlobalEnv
  }

createLabelMap :: Form -> Map Label Form
createLabelMap f =
  let labels = snd (runWriter $ getLabels f)
  in fromList labels

getLabels :: Form -> Writer [(Label, Form)] ()
getLabels f =
  let lab = label $ annotation f
  in case form f of
    Lambda _ bodies -> do
      mapM_ getLabels bodies
      tell [(lab, f)]
    _ ->
      tell [(lab, f)]



-- initialGlobal :: GlobalEnvironment Form
-- initialGlobal =
--   createGlobalEnv (fmap (\(s, p) ->
--                            (s, A primitiveAnnotation . Value . JLProc $ p))
--                     primitiveProcedures)

-- initialState f =
--   AnalysisState
--   { labelMap = createLabelMap f
--   , localEnv = createEmptyEnv
--   -- , globalEnv =k
--   }

data AnalysisError
  = BadNumArguments
  | NotAProcedure
  | UnboundVar
  deriving (Show)


-- I would like the environment to store labels


-- data CFAVal
--   = Foo

-- runCFA :: Form -> Either AnalysisError ([AnalysisState])

-- sweep :: Form -> [AnalysisMonad Form]

-- evaluate f = do
--   undefined
  -- let ann = annotation f
  -- let sp = pos ann
  -- case form f of
  --   v@Value {} ->
  --     return v
  --   Var _ lexAddr -> do
  --     let test =
  --           [ do s <- get
  --                case getValue lexAddr (localEnv s) (globalEnv s) of
  --                  Nothing ->
  --                    AnalysisMonad $ throwE UnboundVar
  --                  Just val ->
  --                    return val
  --           ]
  --       in traceShowM test >> test
  --   Quote _ -> undefined
  --   Lambda fs bs ->
  --     [ do s <- get
  --          return . JLProc $ Closure fs bs (localEnv s) sp
  --     ]
  --   Define var body -> do
  --     [ do undefined
  --       ]
  --   App p args -> do
  --     argEvals <- fmap evaluate args
  --     procEval <- evaluate p
  --     undefined
  --   x -> do
  --     traceShowM x
  --     undefined
