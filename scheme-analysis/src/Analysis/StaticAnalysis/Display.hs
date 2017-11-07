
module Analysis.StaticAnalysis.Display where

import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.AnalysisForms
import Scheme.Types

import Data.Set as S
import Data.List as L

displayTypes :: AnalysisProgram -> AnalysisMonad String
displayTypes (AnalysisProgram frms) = do
  ss <- mapM displayTypesF frms
  return $ unwords ss

displayTypesConst :: Constant -> AnalysisMonad String
displayTypesConst (SInt _) = return $ "{ " ++ show Numeric ++ " }"
displayTypesConst (SNum _) = return $ "{ " ++ show Numeric ++ " }"
displayTypesConst (SStr _) = return $ "{ " ++ show Str     ++ " }"

displayTypeSet :: Set Type -> String
displayTypeSet types =
  "{" ++ L.intercalate ", " (fmap show (S.toList types)) ++ "}"

displayVar :: Ref -> Set Quant -> String -> AnalysisMonad String
displayVar ref qs name = do
  types <- S.unions <$> mapM getQuantTypes (S.toList qs)
  let typesStr = displayTypeSet types
  return $ "[" ++ name ++ "::" ++ typesStr ++ "]"

displayAnalysisFormals :: AnalysisFormals -> AnalysisMonad String
displayAnalysisFormals (AnalysisFormals m) = do
  -- types <- mapM (getRefTypes . snd) m
  -- let typeStrs = fmap displayTypeSet types
  -- let fStrs = fmap (\(v, t) -> v ++ "::" ++ t) (zip (fmap fst m) typeStrs)
  return ("(" ++ unwords (fst <$> m) ++ ")")

displayTypesF :: AnalysisForm -> AnalysisMonad String
displayTypesF (A ann frm) =
  case frm of
    AnalysisConst c ->
      displayTypesConst c
    AnalysisVar name _ ref -> do
      qs <- getQuantsFromRef ref
      displayVar ref qs name
    AnalysisLambda _ formals bodies -> do
      formalsStr <- displayAnalysisFormals formals
      bodiesStr <- unwords <$> mapM displayTypesF bodies
      return $ "(lambda " ++ formalsStr ++ " " ++ bodiesStr ++ ")"
    AnalysisDefine name _ body -> do
      bodyTypes <- displayTypesF body
      return $ "(define " ++ name ++ " " ++ bodyTypes ++ ")"
    AnalysisApp ref rator rands -> do
      ratorS <- displayTypesF rator
      randsS <- mapM displayTypesF rands
      return $ "(" ++ unwords (ratorS:randsS) ++ ")"
