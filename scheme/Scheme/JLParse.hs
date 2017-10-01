
module Scheme.JLParse where

import Scheme.JLTokenize
import Scheme.JLTypes
import Scheme.JLParsingTypes
import {-# SOURCE #-} Scheme.JLPrimitiveSyntax
import Scheme.JLPrimitiveProcs

import Data.Map hiding (foldl, map)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Arrow (second)

import Debug.Trace


invalidSyntax :: JLTree -> Maybe String -> JLSourcePos -> ParseMonad a
invalidSyntax _ Nothing sp =
  ParseMonad . throwE $ JLInvalidSyntax "" sp
invalidSyntax _ (Just s) sp =
  ParseMonad . throwE $ JLInvalidSyntax s sp

unboundVariable :: JLTree -> String -> JLSourcePos -> ParseMonad a
unboundVariable _ name sp =
  ParseMonad . throwE $ JLUndefinedVariable name sp


findIdentifier :: String -> JLEnvironment BoundValue -> Maybe BoundValue
findIdentifier iden (JLEnv m parent) =
  case Data.Map.lookup iden m of
    v@(Just _) -> v
    Nothing -> iden `findIdentifier` parent
findIdentifier _ JLEmptyEnv =
  Nothing

extendEnvironment :: [(String, BoundValue)]
                  -> JLEnvironment BoundValue
                  -> JLEnvironment BoundValue
extendEnvironment ps env =
  let m = fromList ps
  in JLEnv m env

setInEnvironment :: String
                 -> BoundValue
                 -> JLEnvironment BoundValue
                 -> JLEnvironment BoundValue
setInEnvironment name v (JLEnv vs parent) =
  let newMap = insert name v vs
  in JLEnv newMap parent
setInEnvironment _ _ _ = undefined

findInEnvironments :: String
                   -> LocalEnvironment BoundValue
                   -> GlobalEnvironment BoundValue
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

getPairs :: JLTree -> Maybe [(String, JLTree)]
getPairs (JLSList [] _) =
  return []
getPairs (JLSList (JLSList [JLId s _, assgn] _:rest) sp) = do
  ps <- getPairs (JLSList rest sp)
  return $ (s, assgn):ps
getPairs _ = Nothing

getIds :: [JLTree] -> Maybe [String]
getIds [] =
  Just []
getIds (JLId s _:rest) = do
  traceShowM "Here"
  r <- getIds rest
  return $ s:r
getIds _ =
  Nothing

getSourcePos :: JLTree -> JLSourcePos
getSourcePos (JLVal _ sp) = sp
getSourcePos (JLId _ sp) = sp
getSourcePos (JLSList _ sp) = sp

initialGlobal :: GlobalEnvironment BoundValue
initialGlobal =
  let stuff = map (\(s, _) -> (s, BVal)) primitiveProcedures
              ++
              map (second BSyntax) primitiveSyntax
  in GlobalEnv $ JLEnv (fromList stuff) JLEmptyEnv


runJLParse :: String -> Either JLParseError JLProgram
runJLParse s =
  let initialState = ParseState
                     { localEnv = LocalEnv JLEmptyEnv
                     , globalEnv = initialGlobal
                     }
  in do
    tree <- readJL s
    fst $ runIdentity (runStateT (runExceptT (runParser (parse tree)))
                                 initialState)

parse :: [JLTree] -> ParseMonad JLProgram
parse ts =
  JLProgram <$> mapM parseJLForm ts

expandSyntax :: JLSyntax -> JLTree -> ParseMonad JLForm
expandSyntax (BuiltIn _ f) = f

parseJLForm :: JLTree -> ParseMonad JLForm
parseJLForm (JLVal v p) =
  return $ JLValue (JLConst v) p
parseJLForm tree@(JLId x sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  case findInEnvironments x local global of
    Nothing ->
      unboundVariable tree x sp
    Just (BSyntax (BuiltIn name _)) ->
      invalidSyntax tree (Just name) sp
    Just _ ->
      return $ JLVar x sp
parseJLForm tree@(JLSList [] sp) =
  invalidSyntax tree Nothing sp
parseJLForm tree@(JLSList (JLId x idsp:rest) sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  case findInEnvironments x local global of
    Nothing ->
      unboundVariable tree x sp
    Just val ->
      case val of
        BVal -> do
          rexps <- mapM parseJLForm rest
          rx <- parseJLForm $ JLId x idsp
          return $ JLApp rx rexps sp
        BSyntax s ->
          expandSyntax s tree
        _ -> do
          traceShowM val
          undefined
parseJLForm (JLSList (f:rest) sp) = do
  fform <- parseJLForm f
  rforms <- mapM parseJLForm rest
  return $ JLApp fform rforms sp
