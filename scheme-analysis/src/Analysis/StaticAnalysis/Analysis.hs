
{-# LANGUAGE RankNTypes #-}

module Analysis.StaticAnalysis.Analysis
  ( execAnalysis
  , execAnalysisStr
  , getError
  , getProgramErrors
  , generateReport
  )
where

import Scheme.Types ( Constant (..)
                    , Label
                    , Annotated (..)
                    , SourcePos
                    , Program (..)
                    , Annotation
                    )
import Analysis.StaticAnalysis.Types
import Analysis.StaticAnalysis.Patterns
import Analysis.StaticAnalysis.AnalysisForms
import Analysis.StaticAnalysis.Display

import Prelude hiding ( lookup )
import Data.Set as S
import Control.Monad
import Data.Maybe ( catMaybes )

generateReport :: Program Annotation -> Report
generateReport prog =
  let (tProg, parseState) = translateProgram prog
      (_, analysisProg, _) =
        execAnalysisStr tProg parseState
      analysisLog = runDefaultPatterns analysisProg
      errs = getProgramErrors analysisProg
  in Report { errors = errs
            , suggestions = analysisLog
            }

returnConst :: Label -> Type -> AnalysisMonad Quant
returnConst lab t = do
  q <- newLabeledQuant lab
  addTypeToQuant t q
  return q

isError :: Type -> Bool
isError (Error _) = True
isError _ = False

getError :: Type -> Maybe Error
getError (Error e) = Just e
getError _ = Nothing

runConstant :: Constant -> Label -> AnalysisMonad Quant
runConstant (SInt _) lab    = returnConst lab Numeric
runConstant (SNum _) lab    = returnConst lab Numeric
runConstant (SStr _) lab    = returnConst lab Str
runConstant SVoid lab       = returnConst lab Void
runConstant (SBool _) lab   = returnConst lab Boolean
runConstant (SSymbol _) lab = returnConst lab Symbol

bindFormals :: AnalysisFormals -> [Set Quant] -> AnalysisMonad ()
bindFormals (AnalysisFormals refs) args =
  when (length refs == length args)
       (forM_ (zip (fmap snd refs) args) (uncurry addQuantsToRef))
bindFormals _ _ = undefined

applyProc :: SourcePos
          -> Label
          -> Type
          -> [Set Quant]
          -> AnalysisMonad (Set Quant)
applyProc _ _ (Closure (StaticProc ref formals)) ratorQs = do
  bindFormals formals ratorQs
  getQuantsFromRef ref
applyProc sp lab (Closure (StaticPrimitive _ f)) ratorQs = do
  types <- mapM getAllQuantTypes ratorQs
  q <- newLabeledQuant lab
  mapM_ (`addTypeToQuant` q) (S.toList $ f sp types)
  return $ S.singleton q
applyProc sp appLab _ _ = do
  q <- newLabeledQuant appLab
  addTypeToQuant (Error (Err NotAProcedure sp)) q
  return $ S.singleton q

applyProcQ :: SourcePos
           -> Label
           -> Quant
           -> [Set Quant]
           -> AnalysisMonad (Set Quant)
applyProcQ sp lab ratorQ randsQs = do
  qTypes <- getQuantTypes ratorQ
  S.unions <$> mapM (flip (applyProc sp lab) randsQs) (S.toList qTypes)

applyProcSet :: SourcePos
             -> Label
             -> Set Quant
             -> [Set Quant]
             -> AnalysisMonad (Set Quant)
applyProcSet sp lab ratorQs randsQs = do
  results <- mapM (flip (applyProcQ sp lab) randsQs) (S.toList ratorQs)
  return $ S.unions results

runForm :: AnalysisForm -> AnalysisMonad (Set Quant)
runForm (A ann f) =
  let lab = label ann
      sp = sourcePos ann
  in
    case f of
      AnalysisConst c -> do
        q <- runConstant c lab
        return $ S.singleton q

      AnalysisVar _ _ ref ->
        getQuantsFromRef ref

      AnalysisQuote _ -> do
        q <- newLabeledQuant lab
        addTypeToQuant (List UnknownList) q
        return $ S.singleton q

      AnalysisLambda ref formals bodies -> do
        bodyQuants <- mapM runForm bodies
        q <- newLabeledQuant lab
        let t = Closure $ StaticProc ref formals
        addTypeToQuant t q
        addQuantsToRef ref $ last bodyQuants
        return $ S.singleton q

      AnalysisTwoIf ref test true false -> do
        _ <- runForm test
        trueQuants <- runForm true
        falseQuants <- runForm false
        addQuantsToRef ref (trueQuants `S.union` falseQuants)
        getQuantsFromRef ref

      AnalysisDefine _ ref body -> do
        bodyQuants <- runForm body
        addQuantsToRef ref bodyQuants
        return S.empty

      AnalysisApp ref ratorF randsF -> do
        ratorQs <- runForm ratorF
        randsQs <- mapM runForm randsF
        qs <- applyProcSet sp lab ratorQs randsQs
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

      AnalysisQuote _ -> do
        q <- getQuantFromLabel (label ann)
        ts <- getQuantTypes q
        addTypes ts orig

      AnalysisLambda ref formals bodies -> do
        ts <- getRefTypes ref
        bodiesWithTypes <- mapM populateTypes bodies
        return $ A (ann { outTypes = ts })
                   (AnalysisLambda ref formals bodiesWithTypes)

      AnalysisTwoIf ref test true false -> do
        ts <- getRefTypes ref
        testTyped <- populateTypes test
        trueTyped <- populateTypes true
        falseTyped <- populateTypes false
        return $ A (ann { outTypes = ts })
                   (AnalysisTwoIf ref testTyped trueTyped falseTyped)

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

runProgram :: AnalysisProgram -> AnalysisMonad AnalysisProgram
runProgram (AnalysisProgram fs) = do
  replicateM_ 5 (mapM_ runForm fs)
  fsWithTypes <- mapM populateTypes fs
  return $ AnalysisProgram fsWithTypes

runProgramStr :: AnalysisProgram -> AnalysisMonad (String, AnalysisProgram)
runProgramStr p = do
  newP <- runProgram p
  str <- displayTypes newP
  return (str, newP)

execAnalysis :: AnalysisProgram -> ParseState -> AnalysisState
execAnalysis p parseState =
  runAnalysisState p parseState (runProgram p)

execAnalysisStr :: AnalysisProgram
                -> ParseState
                -> (String, AnalysisProgram, AnalysisState)
execAnalysisStr p parseState =
  let ((a, b), c) = runAnalysis p parseState (runProgramStr p)
  in (a, b, c)

getProgramErrors :: AnalysisProgram -> [Error]
getProgramErrors (AnalysisProgram fs) =
  join $ fmap getErrorsF fs

getErrorsF :: AnalysisForm -> [Error]
getErrorsF (A ann f) =
  catMaybes (fmap getError (S.toList (outTypes ann)))
  ++
  join (fmap getErrorsF $ case f of
                            AnalysisLambda _ _ bodies ->
                              bodies
                            AnalysisTwoIf _ test true false ->
                              [ test, true, false ]
                            AnalysisDefine _ _ body ->
                              [ body ]
                            AnalysisApp _ rator rands ->
                              rator : rands
                            _ -> []
       )
