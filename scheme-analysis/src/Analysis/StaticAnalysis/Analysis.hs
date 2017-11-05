
{-# LANGUAGE RankNTypes #-}

module Analysis.StaticAnalysis.Analysis where

import Scheme.Types hiding ( form
                           , Formals
                           )
import Analysis.StaticAnalysis.Types

import Prelude hiding ( lookup )
-- import Safe
import Data.Set as S
import Data.List as L
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

bindFormals :: Formals -> [Set Quant] -> AnalysisMonad ()
bindFormals (Formals qs) args =
  when (length qs == length args) $
       forM_ (zip args qs) (uncurry (flip addQuantsToContour))
bindFormals _ _ = undefined

applyProc :: Label -> Type -> [Set Quant] -> AnalysisMonad (Set Quant)
applyProc _ (StaticProc lab formals) ratorQs = do
  bindFormals formals ratorQs
  getQuantsLabel lab
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

runForm :: Form -> AnalysisMonad (Set Quant)
runForm (A ann f) =
  let lab = label ann
  in
    case f of
      Const c -> do
        q <- runConstant c (label ann)
        return $ S.singleton q
      Var _ addr -> do
        mval <- getEnvValueM addr
        case mval of
          Nothing -> do
            q <- newQuant lab
            addQuantsToAnn ann (S.singleton q)
            return $ S.singleton q
          Just cont ->
            getQuantsFromContour cont
      Define name body -> do
        assignTo <- newContour
        putInGlobalEnv name assignTo
        bodyQuants <- runForm body
        addQuantsToContour assignTo bodyQuants
        return S.empty
      App ratorF randsF -> do
        ratorQs <- runForm ratorF
        randsQs <- mapM runForm randsF
        qs <- applyProcSet lab ratorQs randsQs
        addQuantsToLabel lab qs
        return qs

runProgram :: Program -> AnalysisMonad ()
runProgram (Program fs) =
  mapM_ runForm fs

runProgramStr :: Program -> AnalysisMonad String
runProgramStr p = do
  runProgram p
  displayTypes p

execAnalysis :: Program -> AnalysisState
execAnalysis p =
  runAnalysisState p (runProgram p)

execAnalysisStr :: Program -> (String, AnalysisState)
execAnalysisStr p =
  runAnalysis p (runProgramStr p)

displayTypes :: Program -> AnalysisMonad String
displayTypes (Program frms) = do
  ss <- mapM displayTypesF frms
  return $ unwords ss

displayTypesConst :: Constant -> AnalysisMonad String
displayTypesConst (SInt _) = return $ "{ " ++ show Numeric ++ " }"
displayTypesConst (SNum _) = return $ "{ " ++ show Numeric ++ " }"
displayTypesConst (SStr _) = return $ "{ " ++ show Str     ++ " }"

displayVar :: Set Quant -> String -> AnalysisMonad String
displayVar qs name = do
  types <- S.unions <$> mapM getQuantTypes (S.toList qs)
  let typesStr = "{ " ++ L.intercalate ", " (fmap show (S.toList types)) ++ " }"
  return $ "[ " ++ name ++ " :: " ++ typesStr ++ " ]"

displayTypesF :: Form -> AnalysisMonad String
displayTypesF (A ann frm) =
  case frm of
    Const c ->
      displayTypesConst c
    Define name body -> do
      bodyTypes <- displayTypesF body
      return $ "(define " ++ name ++ " " ++ bodyTypes ++ ")"
    Var name addr -> do
      mcont <- getEnvValueM addr
      case mcont of
        Nothing ->
          displayVar S.empty name
        Just cont -> do
          qs <- getQuantsFromContour cont
          displayVar qs name
    App rator rands -> do
      ratorS <- displayTypesF rator
      randsS <- mapM displayTypesF rands
      return $ "(" ++ unwords (ratorS:randsS) ++ ")"


getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [_] = []
getPairs (a:b:rest) = (a, b) : getPairs rest
