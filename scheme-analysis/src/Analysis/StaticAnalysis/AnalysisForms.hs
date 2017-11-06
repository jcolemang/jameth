
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.StaticAnalysis.AnalysisForms where


import Scheme.Types

import Control.Monad.State
import Control.Monad.Identity

data AnalysisAnnotation
  = AnalysisAnn
  { sourcePos :: SourcePos
  , label :: Label
  }
  deriving ( Show )

newtype Ref
  = Ref Int
  deriving ( Show )

getId :: Ref -> Int
getId (Ref x) = x

instance Eq Ref where
  Ref a == Ref b = a == b

instance Ord Ref where
  Ref a `compare` Ref b = compare a b

newtype AnalysisProgram
  = AnalysisProgram [AnalysisForm]
  deriving ( Show )

type AnalysisForm = Annotated AnalysisAnnotation RawAnalysisForm

data AnalysisFormals
  = AnalysisFormals [(String, Ref)]
  | SymbolFormal Ref
  deriving ( Show )


data RawAnalysisForm
  = AnalysisConst Constant
  | AnalysisVar String LexicalAddress Ref
  | AnalysisLambda Ref
                   AnalysisFormals
                   [AnalysisForm]
  | AnalysisDefine String Ref AnalysisForm
  | AnalysisApp AnalysisForm [AnalysisForm]
  deriving ( Show )

data ParseState
  = ParseState
  { currIdentifier :: Int
  , localEnv :: LocalEnvironment Ref
  , globalEnv :: GlobalEnvironment Ref
  }

newtype AnalysisParse a
  = AnalysisParse
  { runAnalysisParse :: StateT ParseState Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState ParseState
             )

translateProgram :: Program Annotation -> AnalysisProgram
translateProgram prog =
  let initialState = ParseState { currIdentifier = 0
                                , localEnv = createEmptyEnv
                                , globalEnv = createGlobalEnv []
                                }
  in fst $ runIdentity (runStateT (runAnalysisParse (createAnalysisProgram prog)) initialState)

instance Environment AnalysisParse Ref where
  getLocalEnv = localEnv <$> get
  getGlobalEnv = globalEnv <$> get
  putLocalEnv env = modify $ \s -> s { localEnv = env }
  putGlobalEnv env = modify $ \s -> s { globalEnv = env }

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
              , Analysis.StaticAnalysis.AnalysisForms.label =
                  Scheme.Types.label ann
              }

createAnalysisProgram :: Program Annotation -> AnalysisParse AnalysisProgram
createAnalysisProgram (Program fs) =
  AnalysisProgram <$> mapM createAnalysisForm fs

createAnalysisForm :: Form Annotation -> AnalysisParse AnalysisForm
createAnalysisForm (A ann frm) =
  let findOrPut var maddr =
        case maddr of
          Nothing -> do
            r <- newRef
            putInGlobalEnv var r
            return r
          Just addr -> do
            mr <- getEnvValueM addr
            case mr of
              Nothing -> do
                r <- newRef
                putInGlobalEnv var r
                return r
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
                   analysisRator <- createAnalysisForm ratorF
                   analysisRands <- mapM createAnalysisForm randsF
                   return $ AnalysisApp analysisRator analysisRands
    let analysisAnnotation = translateAnnotation ann
    return $ A analysisAnnotation rawForm
