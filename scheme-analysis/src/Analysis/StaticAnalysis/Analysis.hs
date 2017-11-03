
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analysis.StaticAnalysis.Analysis where

import Scheme.Forms hiding ( form )

import Prelude hiding ( lookup )
import Safe
import Data.Map
import Control.Monad.State
import Control.Monad.Identity

data FormState
  = FormState
  { incoming :: [Label]
  , outgoing :: [Label]
  , outputs :: [Form]
  , inputs :: [Form]
  , form :: Form
  } deriving ( Show )

data FlowGraph
  = FlowGraph
  { labels :: [Label]
  , forms :: Map Label FormState
  } deriving ( Show )

newtype AnalysisMonad a
  = AnalysisMonad
  { staticAnalysis :: StateT FlowGraph Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState FlowGraph )

execAnalysis :: AnalysisMonad a -> FlowGraph
execAnalysis am =
  snd $ runIdentity (runStateT (staticAnalysis am) (FlowGraph [] (fromList [])))

putFormState :: Label -> FormState -> AnalysisMonad ()
putFormState lab f =
  modify $ \fg@FlowGraph { labels = l
                         , forms = frms
                         } ->
             fg { labels = lab:l
                , forms = insert lab f frms
                }

putEmptyFormState :: Label -> Form -> AnalysisMonad ()
putEmptyFormState lab frm =
  modify $ \fg@FlowGraph { labels = l
                         , forms = frms
                         } ->
             let f = FormState { incoming = []
                               , outgoing = []
                               , outputs = []
                               , inputs = []
                               , form = frm
                               }
             in
               fg { labels = lab:l
                  , forms = insert lab f frms
                  }

addLink :: (Label, Form) -> (Label, Form) -> AnalysisMonad ()
addLink f@(from, fromForm) t@(to, toForm) = do
  s <- get
  case lookup from (forms s) of
    Nothing ->
      putEmptyFormState from fromForm >> addLink f t
    Just fromFState@FormState { outgoing = fromOG } -> do
      modify $ \fg@FlowGraph { forms = fs } ->
                 fg { forms = insert from
                                     (fromFState { outgoing = to:fromOG })
                                     fs
                    }
      case lookup to (forms s) of
        Nothing ->
          putEmptyFormState to toForm >> addLink f t
        Just toFState@FormState { incoming = toOC } ->
          modify $ \fg@FlowGraph { forms = fs } ->
                    fg { forms = insert to
                                        (toFState { incoming = from:toOC })
                                        fs
                        }

constructFlowGraph :: Program -> AnalysisMonad ()
constructFlowGraph (Program fs) = mapM_ getFormStates fs

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [_] = []
getPairs (a:b:rest) = (a, b) : getPairs rest

-- returns the first thing which will be evaluated
getFormStates :: Form -> AnalysisMonad (Label, Form)
getFormStates frm@(A ann f) =
  let lab = label ann
      me = (lab, frm)
  in
    case f of
      Lambda _ fs -> do
        putEmptyFormState lab frm
        bodies <- mapM getFormStates fs
        mapM_ (uncurry addLink) (getPairs bodies)
        return me
      TwoIf test true false -> do
        putEmptyFormState lab frm
        testLink <- getFormStates test
        trueLink <- getFormStates true
        falseLink <- getFormStates false
        addLink testLink trueLink -- the if is never really evaluated
        addLink testLink falseLink
        return testLink
      Define _ body -> do
        putEmptyFormState lab frm
        b <- getFormStates body
        addLink me b
        return me
      App rator rands -> do
        putEmptyFormState lab frm
        ratLab <- getFormStates rator
        randLabs <- mapM getFormStates rands
        addLink me ratLab
        mapM_ (uncurry addLink) (getPairs (ratLab:randLabs))
        case lastMay randLabs of
          Nothing ->
            addLink ratLab me
          Just v ->
            addLink v me
        return ratLab
      Set _ _ body -> do
        putEmptyFormState lab frm
        b <- getFormStates body
        addLink b me
        return b
      _ -> do
        putEmptyFormState lab frm
        return (lab, frm)
