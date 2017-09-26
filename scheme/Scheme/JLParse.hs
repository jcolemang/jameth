
module Scheme.JLParse where

-- import Scheme.JLGlobalEnvironment
import Scheme.JLTokenize
import Scheme.JLTypes

-- import Data.Maybe
import Data.Map hiding (foldl)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Lens

import Debug.Trace


findIdentifier :: String -> JLEnvironment -> Maybe BoundValue
findIdentifier iden (JLEnvironment m parent) =
  case Data.Map.lookup iden m of
    v@(Just _) -> v
    Nothing -> iden `findIdentifier` parent
findIdentifier _ JLEmptyEnvironment =
  Nothing

extendEnvironment :: [(String, BoundValue)]
                  -> JLEnvironment
                  -> JLEnvironment
extendEnvironment ps env =
  let m = fromList ps
  in JLEnvironment m env

setInEnvironment :: String
                 -> BoundValue
                 -> JLEnvironment
                 -> JLEnvironment
setInEnvironment name v (JLEnvironment vs parent) =
  let newMap = insert name v (trace ("Old Map: " ++ show vs) vs)
  in trace ("Calling set with " ++ name) JLEnvironment newMap parent
setInEnvironment _ _ _ = undefined

findInEnvironments :: String
                   -> LocalEnvironment
                   -> GlobalEnvironment
                   -> Maybe BoundValue
findInEnvironments iden (LocalEnv local) (GlobalEnv global) =
  case findIdentifier iden local of
    Nothing -> findIdentifier iden global
    v@(Just _) -> v

getNums :: [JLValue] -> Maybe [Double]
getNums vals =
  let maybeNum n = case n of
                     JLConst (JLNum x) -> Just x
                     _ -> Nothing
  in mapM maybeNum vals

initialGlobal :: GlobalEnvironment
initialGlobal =
  GlobalEnv . flip JLEnvironment JLEmptyEnvironment . fromList $
  [ ("+", BVal . JLProc . JLPrimitive
      $ \vs ->
          case getNums vs of
            Just nums ->
              return . JLConst . JLNum $ sum nums
            Nothing ->
              EvaluationMonad $
              lift $ throwE (JLTypeError Primitive)
    )
  , ("define", BSyntax . BuiltIn "define" $
      \ts ->
        case ts of
          JLSList [_, JLId x _, jlexp] _ -> do
            traceM "Expanding define form"
            traceM $ "Defining " ++ x
            gEnv <- get
            traceM "Before modify"
            traceShowM gEnv
            modify' $ \(ParseState l (GlobalEnv e)) ->
                        ParseState l (GlobalEnv $ setInEnvironment x EmptySlot e)
            traceM "After modify"
            gEnv' <- get
            traceShowM gEnv'
            pexp <- parseJLForm jlexp
            return . JLFormDef . JLVarDef $ JLDefine x pexp
          _ ->
            undefined
    )
  ]

runJLParse :: String -> Either JLParseError JLProgram
runJLParse s =
  let initialState = ParseState
                     { localEnv = LocalEnv JLEmptyEnvironment
                     , globalEnv = initialGlobal
                     }
  in do
    tree <- readJL s
    fst $ runIdentity (runStateT (runExceptT (runParser (parse tree)))
                       initialState)

parse :: [JLTree] -> ParseMonad JLProgram
parse ts =
  let parseJLForm_ e = trace ("Calling parseJLForm on " ++ show e) (parseJLForm e)
  in
    JLProgram <$> mapM parseJLForm_ ts

expandSyntax :: JLSyntax -> JLTree -> ParseMonad JLForm
expandSyntax (BuiltIn _ f) = f
-- expandSyntax _ _ = undefined

parseJLForm :: JLTree -> ParseMonad JLForm
parseJLForm (JLVal v p) =
  traceM "Found a value" >>
  return . JLFormExp $ JLValue (JLConst v) p
parseJLForm (JLId x sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  traceM ("Current Env: " ++ show global)
  case findInEnvironments x local global of
    Nothing ->
      ParseMonad . throwE $ JLUndefinedVariable x sp
    Just (BSyntax _) ->
      ParseMonad . throwE $ JLInvalidSyntax sp
    Just _ ->
      return . JLFormExp $ JLVar x sp
parseJLForm expr@(JLSList (JLId x idsp:rest) sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  return $ trace ("Current Env: " ++ show global) ()
  case findInEnvironments x local global of
    Nothing ->
      ParseMonad . throwE $ JLUndefinedVariable x sp
    Just val ->
      case val of
        BVal _ -> do
          traceM "Found a value"
          rexps <- mapM parseJLForm rest
          rx <- parseJLForm $ JLId x idsp
          return . JLFormExp $ JLApp rx rexps sp
        BSyntax s ->
          traceM "Found syntax" >>
          expandSyntax s expr
        _ -> undefined
parseJLForm _ =
  undefined


  -- exps <- mapM parseJLForm xs
  -- case listToMaybe exps of
  --   Nothing ->
  --     ParseMonad . throwE $ JLInvalidSyntax sp
  --   Just h ->
  --     return (JLFormExp (JLApp h (tail exps) sp))
