
module Scheme.JLPrimitiveSyntax where

import Scheme.JLParsingTypes
import Scheme.JLTypes
import Scheme.JLParse

whoops :: a
whoops = error "An error was made in the parser. Please report this as a bug."

-- Logic is repeated from the evaluation as the input values are very important
-- in both cases. We can't really factor out the input datatypes.

primitiveSyntax :: [(String, Syntax)]
primitiveSyntax =
  [ ("define", PrimitiveSyntax "define" $
      \ts ->
        case ts of
          JLSList [_, JLId x _, jlexp] sp -> do
            modify' $ \(ParseState l g) ->
                        let (l', g') =
                              putInEnv x
                                       EmptySlot
                                       (globalReference x) l g
                        in ParseState l' g'
            pexp <- parseJLForm jlexp
            modify' $ \(ParseState l g) ->
                        let (l', g') =
                              putInEnv x
                                       (BVal pexp)
                                       (globalReference x) l g
                        in ParseState l' g'
            return $ Define x pexp sp
          JLSList _ sp ->
            invalidSyntax ts (Just "define") sp
          _ ->
            whoops

    )
  , ("lambda", PrimitiveSyntax "lambda" $
    \ts ->
      case ts of
        JLSList [_, JLSList ids _, bodies] sp ->
          case getIds ids of
            Just jids -> do
              let idens = map (\x -> (x, Parameter)) jids
              modify $ \(ParseState l g) ->
                         let newEnv = extendEnv idens l
                         in ParseState newEnv g
              parseJLForm bodies
            Nothing ->
              invalidSyntax ts (Just "lambda") sp
        _ ->
          undefined
    )
  , ("let", PrimitiveSyntax "let" $
    \ts@(JLSList (_:assgns:bs) sp) ->
      case getPairs assgns of
        Just ps -> do
          exps <- mapM (parseJLForm . snd) ps
          let vars = zip (map fst ps) exps
          modify $ \(ParseState l g) ->
            let parsedPairs = zip (fmap fst ps) (fmap BVal exps)
                newEnv = extendEnv parsedPairs l
            in ParseState newEnv g
          bodies <- mapM parseJLForm bs
          return $ Let vars bodies sp
        Nothing ->
          invalidSyntax ts (Just "let") sp
    )
  , ("if", PrimitiveSyntax "if" $
    \x ->
      case x of
        (JLSList [_, test, true, false] sp) -> do
          ptest <- parseJLForm test
          ptrue <- parseJLForm true
          pfalse <- parseJLForm false
          return $ TwoIf ptest ptrue pfalse sp
        (JLSList [_, test, true] sp) -> do
          ptest <- parseJLForm test
          ptrue <- parseJLForm true
          return $ OneIf ptest ptrue sp
        JLSList _ sp ->
          invalidSyntax x (Just "if") sp
        _ ->
          whoops
    )
  , ("quote", PrimitiveSyntax "quote" $
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
           JLSList [_, val] sp ->
             return . flip Quote sp . quote $ val
           JLSList _ sp ->
             invalidSyntax x (Just "quote") sp
           _ ->
             whoops
    )
  ]
