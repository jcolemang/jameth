

{-# LANGUAGE RankNTypes #-}

module Analysis.StaticAnalysis.Analysis where

import Scheme.Types ( Constant (..)
                    , Label
                    , Annotated (..)
                    , Program (..)
                    )
import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.Display

import Prelude hiding ( lookup )
import Data.Set as S
import Control.Monad

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
bindFormals (AnalysisFormals refs) args =
  when (length refs == length args)
       (forM_ (zip (fmap snd refs) args) (uncurry addQuantsToRef))
bindFormals _ _ = undefined

applyProc :: Label -> Type -> [Set Quant] -> AnalysisMonad (Set Quant)
applyProc _ (Closure (StaticProc ref formals)) ratorQs = do
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
        bodyQuants <- mapM runForm bodies
        q <- newQuant lab
        let t = Closure $ StaticProc ref formals
        addTypeToQuant t q
        addQuantsToRef ref $ last bodyQuants
        return $ S.singleton q
      AnalysisDefine _ ref body -> do
        bodyQuants <- runForm body
        addQuantsToRef ref bodyQuants
        return S.empty
      AnalysisApp ref ratorF randsF -> do
        ratorQs <- runForm ratorF
        randsQs <- mapM runForm randsF
        qs <- applyProcSet lab ratorQs randsQs
        addQuantsToRef ref qs
        getQuantsFromRef ref

populateTypes :: AnalysisForm -> AnalysisMonad AnalysisForm
populateTypes orig@(A ann f) =
  let addTypes ts (A ann' raw) =
        return $ A (ann' { outTypes = ts }) raw
  in
    case f of
      AnalysisConst c -> do
        q <- runConstant c (label ann)
        ts <- getQuantTypes q
        addTypes ts orig
      AnalysisVar _ _ ref -> do
        ts <- getRefTypes ref
        addTypes ts orig
      AnalysisLambda ref formals bodies -> do
        ts <- getRefTypes ref
        bodiesWithTypes <- mapM populateTypes bodies
        return $ A (ann { outTypes = ts })
                   (AnalysisLambda ref formals bodiesWithTypes)
      AnalysisDefine name ref body -> do
        bodyWithType <- populateTypes body
        ts <- getRefTypes ref
        return $ A (ann { outTypes = ts })
                   (AnalysisDefine name ref bodyWithType)

      AnalysisApp ref ratorF randsF -> do
        ratorWithTypes <- populateTypes ratorF
        randsWithTypes <- mapM populateTypes randsF
        ts <- getRefTypes ref
        return $ A (ann { outTypes = ts })
                   (AnalysisApp ref ratorWithTypes randsWithTypes)

runProgram :: AnalysisProgram -> AnalysisMonad ()
runProgram (AnalysisProgram fs) =
  replicateM_ 5 (mapM_ runForm fs)

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
