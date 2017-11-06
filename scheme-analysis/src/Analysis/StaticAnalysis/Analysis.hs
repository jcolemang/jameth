
{-# LANGUAGE RankNTypes #-}

module Analysis.StaticAnalysis.Analysis where

import Scheme.Types ( Constant (..)
                    , Label
                    , Annotated (..)
                    , getEnvValueM
                    )
import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.AnalysisForms
import Analysis.StaticAnalysis.Display

import Prelude hiding ( lookup )
import Data.Set as S
import Control.Monad

import Debug.Trace

runConstant :: Constant -> Label -> AnalysisMonad Quant
runConstant (SInt _) lab = do
  q <- newQuant lab
  addTypeToQuant Numeric q
  return q
runConstant (SStr _) lab = do
  q <- newQuant lab
  addTypeToQuant Str q
  return q
runConstant SVoid lab = do
  q <- newQuant lab
  addTypeToQuant Void q
  return q
runConstant _ _ = undefined

bindFormals :: AnalysisFormals -> [Set Quant] -> AnalysisMonad ()
bindFormals (AnalysisFormals refs) args = do
  when (length refs == length args)
       (forM_ (zip (fmap snd refs) args) (uncurry addQuantsToRef))
bindFormals _ _ = undefined

applyProc :: Label -> Type -> [Set Quant] -> AnalysisMonad (Set Quant)
applyProc _ (StaticProc ref formals) ratorQs = do
  bindFormals formals ratorQs
  getQuantsFromRef ref
applyProc appLab _ _ = do
  q <- newQuant appLab
  addTypeToQuant (Error NotAProcedure) q
  return $ S.singleton q

applyProcQ :: Label -> Quant -> [Set Quant] -> AnalysisMonad (Set Quant)
applyProcQ lab ratorQ randsQs = do
  qTypes <- getQuantTypes ratorQ
  S.unions <$> mapM (flip (applyProc lab) randsQs) (S.toList qTypes)

applyProcSet :: Label -> Set Quant -> [Set Quant] -> AnalysisMonad (Set Quant)
applyProcSet lab ratorQs randsQs = do
  results <- mapM (flip (applyProcQ lab) randsQs) (S.toList ratorQs)
  return $ S.unions results

runForm :: AnalysisForm -> AnalysisMonad (Set Quant)
runForm (A ann f) =
  let lab = label ann
  in
    case f of
      AnalysisConst c -> do
        q <- runConstant c (label ann)
        return $ S.singleton q
      AnalysisVar _ _ ref ->
        getQuantsFromRef ref
      AnalysisLambda ref formals bodies -> do
        mapM_ runForm bodies
        q <- newQuant lab
        let t = StaticProc ref formals
        addTypeToQuant t q
        addQuantsToRef ref (S.singleton q)
        getQuantsFromRef ref
      AnalysisDefine _ ref body -> do
        bodyQuants <- runForm body
        addQuantsToRef ref bodyQuants
        return S.empty
      AnalysisApp ratorF randsF -> do
        ratorQs <- runForm ratorF
        randsQs <- mapM runForm randsF
        applyProcSet lab ratorQs randsQs

runProgram :: AnalysisProgram -> AnalysisMonad ()
runProgram (AnalysisProgram fs) =
  mapM_ runForm fs

runProgramStr :: AnalysisProgram -> AnalysisMonad String
runProgramStr p = do
  runProgram p
  displayTypes p

execAnalysis :: AnalysisProgram -> AnalysisState
execAnalysis p =
  runAnalysisState p (runProgram p)

execAnalysisStr :: AnalysisProgram -> (String, AnalysisState)
execAnalysisStr p =
  runAnalysis p (runProgramStr p)
