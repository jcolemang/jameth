
module Scheme.Parse where

import Scheme.Tokenize
import Scheme.Types
import Scheme.JLParsingTypes
import {-# SOURCE #-} Scheme.JLPrimitiveSyntax
import Scheme.JLPrimitiveProcs

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Arrow (second)
import Debug.Trace


invalidSyntax :: JLTree -> Maybe String -> SourcePos -> ParseMonad a
invalidSyntax _ Nothing sp =
  ParseMonad . throwE $ JLInvalidSyntax "" sp
invalidSyntax _ (Just s) sp =
  ParseMonad . throwE $ JLInvalidSyntax s sp

getNums :: [Value] -> Maybe [Double]
getNums vals =
  let maybeNum n = case n of
                     Const (SNum x) -> Just x
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

addGlobalLabel :: String -> RawForm -> ParseMonad Form
addGlobalLabel s f = do
  l1 <- getLabel
  l2 <- getLabel
  let ann1 = Ann PrimitiveSource l1
  let ann2 = Ann PrimitiveSource l2
  return $ A ann1 (Define s (A ann2 f))

primitiveDefinitions :: ParseMonad Program
primitiveDefinitions =
  let fs = map (second $ Value . Proc) primitiveProcedures
  in Program <$> mapM (uncurry addGlobalLabel) fs

initialGlobal :: GlobalEnvironment BoundValue
initialGlobal =
  createGlobalEnv $ fmap (second BSyntax) primitiveSyntax

runParse :: String -> Either JLParseError Program
runParse s =
  let initGlobal = initialState initialGlobal
      addDefs = liftM2 mappend primitiveDefinitions
  in do
    tree <- tokenize s
    fst $ runIdentity (runStateT (runExceptT (runParser (addDefs $ parse tree)))
                                 initGlobal)

runParseNoInit :: String -> Either JLParseError Program
runParseNoInit s =
  let initGlobal = initialState initialGlobal
  in do
    tree <- tokenize s
    fst $ runIdentity (runStateT (runExceptT (runParser (parse tree)))
                                 initGlobal)

parse :: [JLTree] -> ParseMonad Program
parse ts =
  Program <$> mapM parseJLForm ts

expandSyntax :: JLSyntax -> JLTree -> ParseMonad Form
expandSyntax (BuiltIn _ f) = f

parseJLForm :: JLTree -> ParseMonad Form
parseJLForm (JLVal v p) = do
  l <- getLabel
  return $ A (Ann p l) (Value (Const v))

parseJLForm tree@(JLId x sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  l <- getLabel
  case getAddress x local global of
    Nothing ->
      return $ A (Ann sp l) (Var x (globalReference x))
    Just (BSyntax (BuiltIn name _), _) ->
      invalidSyntax tree (Just name) sp
    Just (_, addr) ->
      return $ A (Ann sp l) (Var x addr)

parseJLForm tree@(JLSList [] sp) =
  invalidSyntax tree Nothing sp

-- A separate case is needed so that syntax can be dealt with
parseJLForm tree@(JLSList (ratorT@(JLId x idsp):rest) sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  l <- getLabel
  case getAddress x local global of
    Nothing -> do
      l' <- getLabel
      let rator = A (Ann idsp l) (Var x (globalReference x))
      rands <- mapM parseJLForm rest
      return $ A (Ann sp l') (App rator rands)
    Just (val, _) ->
      case val of
        BVal -> do
          rexps <- mapM parseJLForm rest
          rx <- parseJLForm ratorT
          l' <- getLabel
          return $ A (Ann idsp l') (App rx rexps)
        BSyntax s ->
          expandSyntax s tree
        _ ->
          undefined

parseJLForm (JLSList (f:rest) sp) = do
  fform <- parseJLForm f
  rforms <- mapM parseJLForm rest
  l <- getLabel
  return $ A (Ann sp l) (App fform rforms)
