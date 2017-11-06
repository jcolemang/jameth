
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

displayVar :: Ref -> Set Quant -> String -> AnalysisMonad String
displayVar ref qs name = do
  types <- S.unions <$> mapM getQuantTypes (S.toList qs)
  let typesStr = "{ " ++ L.intercalate ", " (fmap show (S.toList types)) ++ " }"
  return $ "[ " ++ name ++ "@" ++ show (getId ref) ++ " :: " ++ typesStr ++ " ]"

displayAnalysisFormals :: AnalysisFormals -> AnalysisMonad String
displayAnalysisFormals (AnalysisFormals m) =
  return ("(" ++ unwords (fmap fst m) ++ ")")

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
      return $ "(lambda " ++ formalsStr ++ bodiesStr ++ ")"
    AnalysisDefine name _ body -> do
      bodyTypes <- displayTypesF body
      return $ "(define " ++ name ++ " " ++ bodyTypes ++ ")"
    AnalysisApp rator rands -> do
      ratorS <- displayTypesF rator
      randsS <- mapM displayTypesF rands
      return $ "(" ++ unwords (ratorS:randsS) ++ ")"
