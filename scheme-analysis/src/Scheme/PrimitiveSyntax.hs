

module Scheme.PrimitiveSyntax where

import Scheme.ParseTypes
import Scheme.Types
import Scheme.Parse

import Debug.Trace

whoops :: a
whoops = error "An error was made in the parser. Please report this as a bug."

expandLambda :: JLTree -> ParseMonad Form
expandLambda ts =
  case ts of
    JLSList [_, JLSList ids _, bodies] sp ->
      case getIds ids of
        Just jids -> do
          extendEnv $ map (\x -> (x, BVal)) jids
          parsedBodies <- parseJLForm bodies
          popEnv
          l <- getLabel
          return $ A (Ann sp l) (Lambda (Formals jids) [parsedBodies])
        Nothing ->
          invalidSyntax ts (Just "lambda") sp

primitiveSyntax :: [(String, JLSyntax)]
primitiveSyntax =
  [ ("define", BuiltIn "define" $
      \ts ->
        case ts of
          JLSList [_, JLId x _, jlexp] sp -> do
            putInEnv x BVal (globalReference "define")
            pexp <- parseJLForm jlexp
            l <- getLabel
            return $ A (Ann sp l) (Define x pexp)
          JLSList _ sp ->
            invalidSyntax ts (Just "define") sp
          _ ->
            whoops

    )
  , ("lambda", BuiltIn "lambda" expandLambda)
  , ("λ", BuiltIn "Lambda" expandLambda)
  , ("let", BuiltIn "let" $
    \ts@(JLSList (_:assgns:bs) sp) ->
      case getPairs assgns of
        Just ps -> do
          exps <- mapM (parseJLForm . snd) ps
          let vars = zip (map fst ps) exps
          let parsedPairs = map (\(x, _) -> (x, BVal)) ps
          extendEnv parsedPairs
          bodies <- mapM parseJLForm bs
          l <- getLabel
          return $ A (Ann sp l) (Let vars bodies)
        Nothing ->
          invalidSyntax ts (Just "let") sp
    )
  , ("if", BuiltIn "if" $
    \x ->
      case x of
        (JLSList [_, test, true, false] sp) -> do
          ptest <- parseJLForm test
          ptrue <- parseJLForm true
          pfalse <- parseJLForm false
          l <- getLabel
          return $ A (Ann sp l) (TwoIf ptest ptrue pfalse)
        (JLSList [_, test, true] sp) -> do
          l <- getLabel
          ptest <- parseJLForm test
          ptrue <- parseJLForm true
          return $ A (Ann sp l) (OneIf ptest ptrue)
        JLSList _ sp ->
          invalidSyntax x (Just "if") sp
        _ ->
          whoops
    )
  -- , ("quote", BuiltIn "quote" $
  --   let quote x =
  --         case x of
  --           JLSList vals _ ->
  --             VList $ map quote vals
  --           JLVal val _ ->
  --             Const val
  --           JLId s _ ->
  --             Const $ SSymbol s
  --   in \x ->
  --        case x of
  --          JLSList [_, val] sp -> do
  --            l <- getLabel
  --            return $ A (Ann sp l) (Quote $ quote val)
  --            -- return . flip JLQuote sp . quote $ val
  --          JLSList _ sp ->
  --            invalidSyntax x (Just "quote") sp
  --          _ ->
  --            whoops
  --   )
  ]
