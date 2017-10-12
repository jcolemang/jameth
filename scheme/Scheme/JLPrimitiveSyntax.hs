
module Scheme.JLPrimitiveSyntax where

import Scheme.JLParsingTypes
import Scheme.JLTypes
import Scheme.JLParse

whoops :: a
whoops = error "An error was made in the parser. Please report this as a bug."

primitiveSyntax :: [(String, JLSyntax)]
primitiveSyntax =
  [ ("define", BuiltIn "define" $
      \ts ->
        case ts of
          JLSList [_, JLId x _, jlexp] sp -> do
            modify' $ \(ParseState l g lab) ->
                        let (l', g') = putInEnv x BVal (globalReference "define") l g
                        in ParseState l' g' lab
            pexp <- parseJLForm jlexp
            -- modify' $ \(ParseState l g) ->
            --             let (l', g') = putInEnv x BVal (globalReference "define") l g
            --             in ParseState l' g'
            l <- getLabel
            return $ A (Ann sp l) (JLDefine x pexp)
          JLSList _ sp ->
            invalidSyntax ts (Just "define") sp
          _ ->
            whoops

    )
  , ("lambda", BuiltIn "lambda" $
    \ts ->
      case ts of
        JLSList [_, JLSList ids _, bodies] sp ->
          case getIds ids of
            Just jids -> do
              let test = map (\x -> (x, BVal)) jids
              modify $ \(ParseState l g lab) ->
                         let newEnv = extendEnv test l
                         in ParseState newEnv g lab
              parseJLForm bodies
            Nothing ->
              invalidSyntax ts (Just "lambda") sp
        _ ->
          undefined
    )
  , ("let", BuiltIn "let" $
    \ts@(JLSList (_:assgns:bs) sp) ->
      case getPairs assgns of
        Just ps -> do
          exps <- mapM (parseJLForm . snd) ps
          let vars = zip (map fst ps) exps
          modify $ \(ParseState l g lab) ->
            let parsedPairs = map (\(x, _) -> (x, BVal)) ps
                newEnv = extendEnv parsedPairs l
            in ParseState newEnv g lab
          bodies <- mapM parseJLForm bs
          l <- getLabel
          return $ A (Ann sp l) (JLLet vars bodies)
        Nothing ->
          invalidSyntax ts (Just "let") sp
    )
  , ("if", BuiltIn "if" $
    \x ->
      case x of
        (JLSList [_, test, true, false] sp) -> do
          l <- getLabel
          ptest <- parseJLForm test
          ptrue <- parseJLForm true
          pfalse <- parseJLForm false
          return $ A (Ann sp l) (JLTwoIf ptest ptrue pfalse)
        (JLSList [_, test, true] sp) -> do
          l <- getLabel
          ptest <- parseJLForm test
          ptrue <- parseJLForm true
          return $ A (Ann sp l) (JLOneIf ptest ptrue)
        JLSList _ sp ->
          invalidSyntax x (Just "if") sp
        _ ->
          whoops
    )
  , ("quote", BuiltIn "quote" $
    let quote x =
          case x of
            JLSList vals _ ->
              JLList $ map quote vals
            JLVal val _ ->
              JLConst val
            JLId s _ ->
              JLConst $ JLSymbol s
    in \x ->
         case x of
           JLSList [_, val] sp -> do
             l <- getLabel
             return $ A (Ann sp l) (JLQuote $ quote val)
             -- return . flip JLQuote sp . quote $ val
           JLSList _ sp ->
             invalidSyntax x (Just "quote") sp
           _ ->
             whoops
    )
  ]
