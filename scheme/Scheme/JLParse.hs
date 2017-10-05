
module Scheme.JLParse where

import Scheme.JLTokenize
import Scheme.JLTypes
import Scheme.JLParsingTypes
import {-# SOURCE #-} Scheme.JLPrimitiveSyntax
import Scheme.JLPrimitiveProcs

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Arrow (second)


invalidSyntax :: JLTree -> Maybe String -> SourcePos -> ParseMonad a
invalidSyntax _ Nothing sp =
  ParseMonad . throwE $ JLInvalidSyntax "" sp
invalidSyntax _ (Just s) sp =
  ParseMonad . throwE $ JLInvalidSyntax s sp

unboundVariable :: JLTree -> String -> SourcePos -> ParseMonad a
unboundVariable _ name sp =
  ParseMonad . throwE $ JLUndefinedVariable name sp

getNums :: [Value] -> Maybe [Double]
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
  r <- getIds rest
  return $ s:r
getIds _ =
  Nothing

getSourcePos :: JLTree -> SourcePos
getSourcePos (JLVal _ sp) = sp
getSourcePos (JLId _ sp) = sp
getSourcePos (JLSList _ sp) = sp

initialGlobal :: GlobalEnvironment BoundValue
initialGlobal =
  let stuff = map (\(s, _) -> (s, BVal)) primitiveProcedures
              ++
              map (second BSyntax) primitiveSyntax
  in createGlobalEnv stuff


runJLParse :: String -> Either JLParseError Program
runJLParse s =
  let initialState = ParseState
                     { localEnv = createEmptyEnv
                     , globalEnv = initialGlobal
                     }
  in do
    tree <- readJL s
    fst $ runIdentity (runStateT (runExceptT (runParser (parse tree)))
                                 initialState)

parse :: [JLTree] -> ParseMonad Program
parse ts =
  Program <$> mapM parseJLForm ts

expandSyntax :: JLSyntax -> JLTree -> ParseMonad Form
expandSyntax (BuiltIn _ f) = f

parseJLForm :: JLTree -> ParseMonad Form
parseJLForm (JLVal v p) =
  return $ Value (JLConst v) p
parseJLForm tree@(JLId x sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  case getAddress x local global of
    Nothing ->
      unboundVariable tree x sp
    Just (BSyntax (BuiltIn name _), _) ->
      invalidSyntax tree (Just name) sp
    Just (_, addr) ->
      return $ Var x addr sp
parseJLForm tree@(JLSList [] sp) =
  invalidSyntax tree Nothing sp
parseJLForm tree@(JLSList (JLId x idsp:rest) sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  case getAddress x local global of
    Nothing ->
      unboundVariable tree x sp
    Just (val, _) ->
      case val of
        BVal -> do
          rexps <- mapM parseJLForm rest
          rx <- parseJLForm $ JLId x idsp
          return $ JLApp rx rexps sp
        BSyntax s ->
          expandSyntax s tree
        _ ->
          undefined
parseJLForm (JLSList (f:rest) sp) = do
  fform <- parseJLForm f
  rforms <- mapM parseJLForm rest
  return $ JLApp fform rforms sp
