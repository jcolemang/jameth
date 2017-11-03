
module Scheme.PrimitiveSyntax where

import Scheme.ParseTypes
import Scheme.Types
import Scheme.Parse

import Debug.Trace

whoops :: a
whoops = error "An error was made in the parser. Please report this as a bug."

allIds :: [Tree] -> Maybe [String]
allIds [] = Just []
allIds (TreeId x _:rest) = (x:) <$> allIds rest
allIds _ = Nothing

getFormals :: [Tree] -> Maybe [String]
getFormals [] = Just []
getFormals t@(TreeId s _:rest) = do
  restFormals <- getFormals rest
  return $ s : restFormals
getFormals _ = Nothing

getPairs :: Tree -> Maybe [(String, Tree)]
getPairs (TreeSList [] _) = return []
getPairs (TreeSList (TreeSList [TreeId s _, x] sp:rest) _) = do
  rps <- getPairs (TreeSList rest sp)
  return ((s, x):rps)
getPairs _ = Nothing

expandLambda :: Tree -> ParseMonad Form
expandLambda ts =
  case ts of
    TreeSList [_, TreeSList ids _, bodies] sp ->
      case getFormals ids of
        Just fsList -> do
          extendEnv $ map (\x -> (x, BVal)) fsList
          parsedBodies <- parseForm bodies
          popEnv
          l <- getLabel
          let formals = Formals fsList
          return $ A (Ann sp l ts) (Lambda formals [parsedBodies])
        Nothing ->
          invalidSyntax ts (Just "lambda") sp

primitiveSyntax :: [(String, Syntax)]
primitiveSyntax =
  [ ("define", BuiltIn "define" $
      \ts ->
        case ts of
          TreeSList [_, TreeId x _, jlexp] sp -> do
            putInEnv x BVal (globalReference x)
            pexp <- parseForm jlexp
            l <- getLabel
            return $ A (Ann sp l ts) (Define x pexp)
          TreeSList [_, TreeSList (TreeId x _:args) asp, jlexp] sp -> do
            putInEnv x BVal (globalReference x)
            l <- getLabel
            lam <- expandLambda (TreeSList [ TreeId "lambda" sp
                                           , TreeSList args asp
                                           , jlexp ] sp)
            return $ A (Ann sp l ts) (Define x lam)
          TreeSList _ sp ->
            invalidSyntax ts (Just "define") sp
          _ ->
            whoops

    )
  , ("lambda", BuiltIn "lambda" expandLambda)
  , ("λ", BuiltIn "lambda" expandLambda)
  , ("let", BuiltIn "let" $
    \ts@(TreeSList (_:assgns:bs) sp) ->
      case getPairs assgns of
        Just ps -> do
          let parsedPairs = map (\(x, _) -> (x, BVal)) ps
          exps <- mapM (parseForm . snd) ps
          extendEnv parsedPairs
          local <- getLocalEnv

          body <- parseForm (head bs)
          popEnv
          lamLabel <- getLabel
          let lam = A (Ann sp lamLabel ts) (Lambda (Formals (fmap fst ps)) [body])
          l <- getLabel
          return $ A (Ann sp l ts) (App lam exps)
        Nothing ->
          invalidSyntax ts (Just "let") sp
    )
  , ("if", BuiltIn "if" $
    \x ->
      case x of
        (TreeSList [_, test, true, false] sp) -> do
          ptest <- parseForm test
          ptrue <- parseForm true
          pfalse <- parseForm false
          l <- getLabel
          return $ A (Ann sp l x) (TwoIf ptest ptrue pfalse)
        (TreeSList [_, test, true] sp) -> do
          l <- getLabel
          ptest <- parseForm test
          ptrue <- parseForm true
          ann <- expandedAnn x
          return $ A (Ann sp l x) (TwoIf ptest ptrue (A ann (Const SVoid)))
        TreeSList _ sp ->
          invalidSyntax x (Just "if") sp
        _ ->
          whoops
    )
  , ("quote", BuiltIn "quote" $
    let quote x =
          case x of
            TreeSList vals _ ->
              SList $ map quote vals
            TreeVal val _ ->
              Constant val
            TreeId s _ ->
              Symbol s
    in \x ->
         case x of
           TreeSList [_, val] sp -> do
             l <- getLabel
             return $ A (Ann sp l x) (Quote $ quote val)
             -- return . flip JLQuote sp . quote $ val
           TreeSList _ sp ->
             invalidSyntax x (Just "quote") sp
           _ ->
             whoops
    )
  , ("set!", BuiltIn "set!" $
    \x ->
      case x of
        TreeSList [_, TreeId name _, body] sp -> do
          l <- getLabel
          local <- getLocalEnv
          global <- getGlobalEnv
          frm <- parseForm body
          case getAddress name local global of
            Nothing ->
              undefined
            Just addr ->
              let annot = Ann sp l x
              in return $ A annot (Set name addr frm)
    )
  ]
