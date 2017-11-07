
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.StaticAnalysis.AnalysisForms where

import Scheme.PrimitiveProcedures
import Analysis.StaticAnalysis.AnalysisPrimitives
import Analysis.StaticAnalysis.Types
import Scheme.Types hiding ( Closure
                           )

import Control.Monad.State
import Control.Monad.Identity
import Data.Set as S
import Data.Map as M

import Debug.Trace

initializeGlobal :: AnalysisParse ()
initializeGlobal =
  let pairs         = primitiveProcedures
      primFuncs     = convertPrimitive <$> pairs
      funcsWithStrs = zip (fst <$> pairs) primFuncs
      staticPrims   = uncurry StaticPrimitive <$> funcsWithStrs
      closures      = zip (fst <$> pairs) (Closure <$> staticPrims)
  in traceShowM "HERE I AM" >> forM_ closures $ \(name, c) -> do
    q <- newGlobalQuant name
    addTypeToQuant c q
    r <- newRef
    addQuantsToRef r (S.singleton q)
    putInGlobalEnv name r

translateProgram :: Program Annotation -> (AnalysisProgram, ParseState)
translateProgram prog = execAnalysisParse $ do
  initializeGlobal
  createAnalysisProgram prog

execAnalysisParse :: AnalysisParse a -> (a, ParseState)
execAnalysisParse m =
  let initialState = ParseState { currIdentifier = 0
                                , parseQuantNum  = 0
                                , localEnv       = createEmptyEnv
                                , globalEnv      = createGlobalEnv []
                                , parseRefs      = M.empty
                                , parseValues    = M.empty
                                , globalQuants   = M.empty
                                }
  in runIdentity (
    runStateT (
        runAnalysisParse m
        )
      initialState
    )

formalsMap :: AnalysisFormals -> [(String, Ref)]
formalsMap (AnalysisFormals m) = m

newRef :: AnalysisParse Ref
newRef = do
  num <- currIdentifier <$> get
  modify $ \s -> s { currIdentifier = num + 1 }
  return $ Ref num

translateFormals :: Formals -> AnalysisParse AnalysisFormals
translateFormals (Formals names) = do
  refs <- mapM (const newRef) [1..length names]
  return $ AnalysisFormals (zip names refs)

translateAnnotation :: Annotation -> AnalysisAnnotation
translateAnnotation ann =
  AnalysisAnn { sourcePos = pos ann
              , Analysis.StaticAnalysis.Types.label =
                  Scheme.Types.label ann
              , outTypes = S.empty
              }

createAnalysisProgram :: Program Annotation -> AnalysisParse AnalysisProgram
createAnalysisProgram (Program fs) =
  AnalysisProgram <$> mapM createAnalysisForm fs

createAnalysisForm :: Form Annotation -> AnalysisParse AnalysisForm
createAnalysisForm (A ann frm) =
  let addRef var = do
        r <- newRef
        putInGlobalEnv var r
        return r
      findOrPut var maddr =
        case maddr of
          Nothing ->
            addRef var
          Just addr -> do
            mr <- getEnvValueM addr
            case mr of
              Nothing ->
                addRef var
              Just r ->
                return r
  in do
    rawForm <- case frm of
                 Const c ->
                   return $ AnalysisConst c
                 Var name addr -> do
                   mVal <- getEnvValueM addr
                   case mVal of
                     Nothing -> do
                       ref <- newRef
                       return $ AnalysisVar name addr ref
                     Just ref ->
                       return $ AnalysisVar name addr ref
                 Lambda formals bodies -> do
                   ref <- newRef
                   analysisFormals <- translateFormals formals
                   withExtendedEnv (formalsMap analysisFormals) $ do
                     translatedBodies <- mapM createAnalysisForm bodies
                     return $ AnalysisLambda ref
                                             analysisFormals
                                             translatedBodies
                 Define var body -> do
                   mAddr <- getAddressM var
                   ref <- findOrPut var mAddr
                   analysisBody <- createAnalysisForm body
                   return $ AnalysisDefine var ref analysisBody
                 App ratorF randsF -> do
                   ref <- newRef
                   analysisRator <- createAnalysisForm ratorF
                   analysisRands <- mapM createAnalysisForm randsF
                   return $ AnalysisApp ref analysisRator analysisRands
    let analysisAnnotation = translateAnnotation ann
    return $ A analysisAnnotation rawForm
