
module Scheme.JLPrimitiveSyntax where

import Scheme.JLParsingTypes
import Scheme.JLTypes
import Scheme.JLParse

primitiveSyntax :: [(String, JLSyntax)]
primitiveSyntax =
  [ ("define", BuiltIn "define" $
      \ts ->
        case ts of
          JLSList [_, JLId x _, jlexp] _ -> do
            modify' $ \(ParseState l (GlobalEnv e)) ->
                        ParseState l (GlobalEnv $ setInEnvironment x BVal e)
            pexp <- parseJLForm jlexp
            modify' $ \(ParseState l (GlobalEnv e)) ->
                        ParseState l (GlobalEnv $ setInEnvironment x BVal e)
            return $ JLDefine x pexp
          JLSList _ sp ->
            invalidSyntax ts (Just "define") sp
          _ ->
            error "An error was made in the parser. Please report this as a bug."
    )
  , ("lambda", BuiltIn "lambda" $
    \ts ->
      case ts of
        JLSList [_, JLSList ids idsSp, bodies] sp ->
          case getIds ids of
            Just jids -> do
              let test = map (\x -> (x, BVal)) jids
              modify $ \(ParseState (LocalEnv l) g) ->
                         let newEnv = (LocalEnv $ extendEnvironment test l)
                         in ParseState newEnv g
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
          modify $ \(ParseState (LocalEnv l) g) ->
            let parsedPairs = map (\(x, _) -> (x, BVal)) ps
                newEnv = (LocalEnv $ extendEnvironment parsedPairs l)
            in ParseState newEnv g
          bodies <- mapM parseJLForm bs
          return $ JLLet vars bodies sp
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
          return $ JLTwoIf ptest ptrue pfalse sp
        _ ->
          undefined
    )
  -- , ("quote", BSyntax . BuiltIn "quote" $
  --   \x ->
  --     case x of
  --       JLSList (_:vals) sp ->
  --         return $ JLQuoted (JLSList vals sp) sp
  --       a ->
  --         invalidSyntax x (Just "quote") (getSourcePos a)
  --   )
  ]
