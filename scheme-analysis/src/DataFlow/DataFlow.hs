
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataFlow.DataFlow where

import Scheme.JLTypes
import Scheme.JLPrimitiveProcs

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

-- runAnalysis :: AnalysisMonad a -> Either AnalysisError (s, a)
-- runAnalysis am =
--   runIdentity (runStateT)

data AnalysisState
  = AnalysisState
  { labelMap :: Map Label Form
  , localEnv :: LocalEnvironment Form
  , globalEnv :: GlobalEnvironment Form
  }

-- createLabelMap :: Form -> Map Label Form
-- createLabelMap f =
--   let labels = snd (runWriter $ getLabels f)
--   in fromList labels

getLabels :: Form -> Writer [(Label, Form)] ()
getLabels f =
  let lab = label $ annotation f
  in case form f of
    Lambda _ bodies -> do
      mapM_ getLabels bodies
      tell [(lab, f)]
    x -> do
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


-- I would like the environment to store labels


data CFAVal
  = Foo

-- runCFA :: Form -> Either AnalysisError ([AnalysisState])

cfa :: Form -> [AnalysisMonad Form]
cfa f = do
  let ann = annotation f
  let sp = pos ann
  case form f of
    -- v@Value {} ->
    --   return v
    -- Var _ lexAddr -> do
    --   let test =
    --         [ do s <- get
    --              case getValue lexAddr (localEnv s) (globalEnv s) of
    --                Nothing ->
    --                  AnalysisMonad $ throwE UnboundVar
    --                Just val ->
    --                  return val
    --         ]
    --     in traceShowM test >> test
    -- Quote _ -> undefined
    -- Lambda fs bs ->
    --   [ do s <- get
    --        return . JLProc $ Closure fs bs (localEnv s) sp
    --   ]
    -- Let bindings bs ->
    --   let lambdaPart =
    --         A ann (Lambda (Formals $ fmap fst bindings) bs)
    --       expanded =
    --         A ann (App lambdaPart (fmap snd bindings))
    --   in cfa expanded
    TwoIf test true false -> do
      testEval <- cfa test
      join [ do trueEval <- cfa true
                [ testEval >> trueEval ]
           , do falseEval <- cfa false
                [ testEval >> falseEval ]
           ]
  -- m a -> (a -> m b) -> m b
      -- [ [testEval] >> cfa true
      --   , testEval >> cfa false
      --   ]
      -- mappend (cfa true) (cfa false)
    OneIf test true -> do
      _ <- cfa test
      cfa true
    -- Define var body -> do
    --   undefined
    App p args -> do
      argEvals <- fmap cfa args
      procEval <- cfa p


      undefined
    _ -> undefined
