
module Scheme.Parse
  ( runParse
  , parseForm
  , invalidSyntax
  )
where

import Scheme.Tokenize
import Scheme.Types
import Scheme.ParseTypes
import {-# SOURCE #-} Scheme.PrimitiveSyntax
import Scheme.PrimitiveProcedures

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Arrow (second)

import Debug.Trace


invalidSyntax :: Tree -> Maybe String -> SourcePos -> ParseMonad a
invalidSyntax _ Nothing sp =
  ParseMonad . throwE $ InvalidSyntax "" sp
invalidSyntax _ (Just s) sp =
  ParseMonad . throwE $ InvalidSyntax s sp

getNums :: [Constant] -> Maybe [Double]
getNums vals =
  let maybeNum n = case n of
                     (SNum x) -> Just x
                     _ -> Nothing
  in mapM maybeNum vals

getPairs :: Tree -> Maybe [(String, Tree)]
getPairs (TreeSList [] _) =
  return []
getPairs (TreeSList (TreeSList [TreeId s _, assgn] _:rest) sp) = do
  ps <- getPairs (TreeSList rest sp)
  return $ (s, assgn):ps
getPairs _ = Nothing

getIds :: [Tree] -> Maybe [String]
getIds [] =
  Just []
getIds (TreeId s _:rest) = do
  r <- getIds rest
  return $ s:r
getIds _ =
  Nothing

getSourcePos :: Tree -> SourcePos
getSourcePos (TreeVal _ sp) = sp
getSourcePos (TreeId _ sp) = sp
getSourcePos (TreeSList _ sp) = sp

-- addGlobalLabel :: String -> RawForm -> ParseMonad Form
-- addGlobalLabel s f = do
--   l1 <- getLabel
--   l2 <- getLabel
--   let ann1 = Ann PrimitiveSource l1
--   let ann2 = Ann PrimitiveSource l2
--   return $ A ann1 (Define s (A ann2 f))

-- primitiveDefinitions :: ParseMonad Program
-- primitiveDefinitions =
--   let fs = map (second $ Value . Proc) primitiveProcedures
--   in Program <$> mapM (uncurry addGlobalLabel) fs

initialGlobal :: GlobalEnvironment BoundValue
initialGlobal =
  createGlobalEnv $ join [ fmap (second BSyntax) primitiveSyntax
                         , fmap (second (const BVal)) primitiveProcedures
                         ]

runParse :: String -> Either ParseError Program
runParse s =
  let initGlobal = initialState initialGlobal
  in do
    tree <- tokenize s
    fst $ runIdentity (runStateT (runExceptT (runParser (parse tree)))
                                 initGlobal)

parse :: [Tree] -> ParseMonad Program
parse ts =
  Program <$> mapM parseForm ts

expandSyntax :: Syntax -> Tree -> ParseMonad Form
expandSyntax (BuiltIn _ f) = f

parseForm :: Tree -> ParseMonad Form
parseForm t@(TreeVal v p) = do
  l <- getLabel
  return $ A (Ann p l t) (Const v)

parseForm tree@(TreeId x sp) = do
  global <- getGlobalEnv
  local <- getLocalEnv
  l <- getLabel
  case getVar x local global of
    Nothing ->
      return $ A (Ann sp l tree) (Var x (globalReference x))
    Just (BSyntax (BuiltIn name _), _) ->
      invalidSyntax tree (Just name) sp
    Just (_, addr) ->
      return $ A (Ann sp l tree) (Var x addr)

parseForm tree@(TreeSList [] sp) =
  invalidSyntax tree Nothing sp

-- A separate case is needed so that syntax can be dealt with
parseForm tree@(TreeSList (ratorT@(TreeId x idsp):rest) sp) = do
  local <- localEnv <$> get
  global <- globalEnv <$> get
  l <- getLabel
  case getVar x local global of
    Nothing -> do
      l' <- getLabel
      let rator = A (Ann idsp l ratorT) (Var x (globalReference x))
      rands <- mapM parseForm rest
      return $ A (Ann sp l' tree) (App rator rands)
    Just (val, _) ->
      case val of
        BVal -> do
          rexps <- mapM parseForm rest
          rx <- parseForm ratorT
          return $ A (Ann idsp l tree) (App rx rexps)
        BSyntax s ->
          expandSyntax s tree
        _ ->
          undefined

parseForm t@(TreeSList (f:rest) sp) = do
  fform <- parseForm f
  rforms <- mapM parseForm rest
  l <- getLabel
  return $ A (Ann sp l t) (App fform rforms)
