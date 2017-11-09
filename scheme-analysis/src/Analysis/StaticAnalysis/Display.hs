
module Analysis.StaticAnalysis.Display where

import Analysis.StaticAnalysis.Types
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
displayTypesConst (SBool _) = return $ "{ " ++ show Boolean     ++ " }"
displayTypesConst SVoid = return $ "{ " ++ show Void     ++ " }"
displayTypesConst (SSymbol _) = return $ "{ " ++ show Void     ++ " }"

displayTypeSet :: Set Type -> String
displayTypeSet types =
  "{" ++ L.intercalate ", " (fmap show (S.toList types)) ++ "}"

displayVar :: Ref -> Set Quant -> String -> AnalysisMonad String
displayVar (Ref refNum) qs name = do
  types <- S.unions <$> mapM getQuantTypes (S.toList qs)
  let typesStr = displayTypeSet types
  return $ "[" ++ name ++ "@" ++ show refNum ++ "::" ++ typesStr ++ "]"

displayAnalysisFormals :: AnalysisFormals -> AnalysisMonad String
displayAnalysisFormals (AnalysisFormals m) = do
  types <- mapM (getRefTypes . snd) m
  let typeStrs = fmap displayTypeSet types
  let fStrs = fmap (\((v, Ref r), t) -> v ++ "@"
                                          ++ show r
                                          ++ "::"
                                          ++ t) (zip m typeStrs)
  return ("(" ++ unwords fStrs ++ ")")

displayTypesF :: AnalysisForm -> AnalysisMonad String
displayTypesF (A _ frm) =
  case frm of
    AnalysisConst c ->
      displayTypesConst c
    AnalysisVar name _ ref -> do
      qs <- getQuantsFromRef ref
      displayVar ref qs name
    AnalysisQuote slist ->
      return $ displaySList slist
    AnalysisLambda _ formals bodies -> do
      formalsStr <- displayAnalysisFormals formals
      bodiesStr <- unwords <$> mapM displayTypesF bodies
      return $ "(lambda " ++ formalsStr ++ " " ++ bodiesStr ++ ")"
    AnalysisTwoIf _ test true false -> do
      testStr <- displayTypesF test
      trueStr <- displayTypesF true
      falseStr <- displayTypesF false
      return $ "(if " ++ testStr ++ " " ++ trueStr ++ " " ++ falseStr ++ ")"
    AnalysisDefine name _ body -> do
      bodyTypes <- displayTypesF body
      return $ "(define " ++ name ++ " " ++ bodyTypes ++ ")"
    AnalysisApp _ rator rands -> do
      ratorS <- displayTypesF rator
      randsS <- mapM displayTypesF rands
      return $ "(" ++ unwords (ratorS:randsS) ++ ")"
