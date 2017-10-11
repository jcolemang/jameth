
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheme.JLEvaluationTypes
  ( evaluate
  , runEvalT
  , Bindable (..)
  , EvaluationMonadT (..)
  , EvaluationState
  , EvaluationError (..)
  )
where

import Scheme.JLTypes
import Scheme.JLParse

import Control.Monad.State hiding (void)
import Control.Monad.Except hiding (void)
import Control.Monad.Writer hiding (void)
import Data.List

newtype EvaluationMonadT m s w a
  = EvalMonadT
  { evalT :: WriterT w (ExceptT EvaluationError (StateT (EvaluationState s) m)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter w
             , MonadState (EvaluationState s)
             , MonadError EvaluationError )

runEvalT m =
  let initialState = EvalState { localEnv = createEmptyEnv
                               , globalEnv = initialGlobal
                               }
  in runStateT (runExceptT (runWriterT (evalT m))) initialState

data EvaluationError
  = JLEvalError         SourcePos
  | JLUndefined         SourcePos
  | JLTypeError         SourcePos
  | JLNotAProcedure     SourcePos
  | JLBadNumArgs        SourcePos SourcePos
  | JLNoBodies          SourcePos
  deriving (Show)

data EvaluationState a
  = EvalState
  { localEnv :: LocalEnvironment a
  , globalEnv :: GlobalEnvironment a
  } deriving (Show)

class Bindable a where
  emptySlot :: a
  lambda :: Formals -> [Form] -> SourcePos -> a
  ifThenElse :: Form -> Form -> Form -> SourcePos -> a
  apply :: a -> [a] -> SourcePos -> a
  void :: a

-- Almost exactly bind, but this depends on the value wrapped in the monad
-- and so will not satisfy the monad laws
evaluate :: ( Monad m
            , Bindable a
            , MonadError EvaluationError m
            , MonadState (EvaluationState a) m
            )
         => Form
         -> (Form -> m a)
         -> m a
evaluate frm@Value {} fun = fun frm
evaluate frm@Var {} fun = fun frm
evaluate frm@Quote {} fun = fun frm
evaluate frm@(Lambda formals bodies sp) fun = do
  _ <- fun frm
  mapM_ fun bodies
  return $ lambda formals bodies sp
evaluate frm@(Let bindings bodies sp) fun = do
  _ <- fun frm
  vals <- mapM (flip evaluate fun . snd) bindings
  let vars = fmap fst bindings
  modify $ \(EvalState l g) ->
             let l' = extendEnv (zip vars vals) l
             in EvalState l' g
  evaled <- mapM fun bodies
  case uncons $ reverse evaled of
    Nothing -> throwError $ JLNoBodies sp
    Just (v, _) -> return v
evaluate frm@(TwoIf test true false sp) fun =
  -- testVal <- evaluate
  fun frm >> return (ifThenElse test true false sp)
evaluate frm@(OneIf test true sp) fun =
  fun frm >> return (ifThenElse test true (flip Value sp $ JLConst JLVoid) sp)
evaluate frm@(Define s f _) fun = do
  _ <- fun frm
  modify $ \(EvalState l g) ->
             let (l', g') = putInEnv s
                                     emptySlot
                                     (globalReference s) l g
             in EvalState l' g'
  val <- evaluate f fun
  modify $ \(EvalState l g) ->
             let (l', g') = putInEnv s
                                     val
                                     (globalReference s) l g
             in EvalState l' g'
  return void
evaluate v@(App form as sp) fun = do
  _ <- fun v
  f <- fun form
  args <- mapM fun as
  return $ apply f args sp
