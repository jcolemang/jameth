
module JLEval where

import Control.Monad
import JLTypes
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Map
import Control.Lens


-- | Primitive procudures

jlAdd2 (JLInt a) (JLInt b) = return . JLInt $ a + b
jlAdd2 (JLNum a) (JLInt b) = return . JLNum $ a + fromInteger b
jlAdd2 (JLInt a) (JLNum b) = return . JLNum $ fromInteger a + b
jlAdd2 (JLNum a) (JLNum b) = return . JLNum $ a + b
jlAdd2 _ _ = throwE JLUndefined

jlAdd :: [JLValue] -> Evaluation
jlAdd [] = return $ JLInt 0
jlAdd [x@(JLInt _)] = return x
jlAdd [x@(JLNum _)] = return x
jlAdd nums = lift $ foldM jlAdd2 (JLInt 0) nums


-- | Helpers

booleanValue :: JLValue -> Bool
booleanValue (JLBool False) = False
booleanValue _ = False


initialEnvironment :: Environment
initialEnvironment = GlobalEnv . fromList $
  [ ("+", JLProc $ JLPrimitive jlAdd) ]


initialState :: EvaluationState
initialState = EvaluationState
  { _environment = initialEnvironment }


bindArguments :: (Monad m)
              => JLFormals
              -> [JLValue]
              -> ExceptT EvaluationError m (Map String JLValue)
bindArguments (JLSymbolFormal s) values  =
  return $ singleton s (JLList values)
bindArguments (JLFormals ss) values =
  if length ss == length values
  then return $ fromList (zip ss values)
  else throwE JLBadArgumentLength
bindArguments (JLImproperFormals f mid l) values =
  let (vf, vl) = splitAt (length (f:mid)) values
      most = zip (f:mid) vf
      rest = [(l, JLList vl)]
  in return $ fromList (mappend most rest)


applyClosure :: [JLValue] -> JLClosure -> Evaluation
applyClosure vals (JLPrimitive f) = f vals
applyClosure vals (JLClosure formals body _) = do
  args <- lift $ bindArguments formals vals
  modify $ over environment (LocalEnv args)
  evalBody body


lookupEnv :: (Monad m) => String -> Environment -> MaybeT m JLValue
lookupEnv x (LocalEnv m parent) =
  case Data.Map.lookup x m of
    Nothing -> lookupEnv x parent
    Just val -> return val
lookupEnv x (GlobalEnv m) =
  case Data.Map.lookup x m of
    Nothing -> mzero
    Just val -> return val


-- | Actual Evaluation

evalProgramT :: JLProgram -> Evaluation
evalProgramT (JLProgram forms) =
  foldM (\_ next -> evalForm next) JLVoid forms

evalProgram :: JLProgram -> IO (Either EvaluationError JLValue)
evalProgram code =
  let result = evalProgramT code
  in runExceptT $ evalStateT result initialState

evalBody :: JLBody -> Evaluation
evalBody (JLBody defs (fexp, frest)) = do
  foldM (\_ d -> evalDefinition d) JLVoid defs
  foldM (\_ e -> evalExpression e) JLVoid (fexp:frest)

evalDefinition :: JLDefinition -> Evaluation
evalDefinition =
  undefined




evalForm :: JLForm
         -> Evaluation
evalForm (JLFormExp expr) = evalExpression expr


evalExpression :: JLExpression -> Evaluation
evalExpression (JLValue val _) =
  return val
evalExpression (JLVar x _) = do
  e <- gets _environment
  lift $ maybeToExceptT JLUndefined (lookupEnv x e)
evalExpression (JLQuote _ _) =
  undefined
evalExpression (JLLambda formals body p) =
  return . JLProc $ JLClosure formals body p
evalExpression (JLTwoIf condexp thenexp elseexp _) = do
  condVal <- evalExpression condexp
  if booleanValue condVal
    then evalExpression thenexp
    else evalExpression elseexp
evalExpression (JLOneIf condexp thenexp _) = do
  condVal <- evalExpression condexp
  if booleanValue condVal
    then evalExpression thenexp
    else return JLVoid
evalExpression (JLApp f args _) = do
  f' <- evalExpression f
  args' <- mapM evalExpression args
  case f' of
    JLProc c -> applyClosure args' c
    _ -> lift $ throwE JLNotAProcedure
