
module Scheme.Tokenize (tokenize,
                       ) where

import Scheme.ParseTypes
import Scheme.Types

import Text.ParserCombinators.Parsec hiding (ParseError)
import Data.List
import Control.Monad

tokenize :: String -> Either ParseError [Tree]
tokenize =
  tokenize' . removeAllComments

tokenize' :: String -> Either ParseError [Tree]
tokenize' s =
  case parse (whitespaces *> many parseTree <* whitespaces <* eof) "jameth" s of
    Left p ->
      let ep = errorPos p
          posn = SP (sourceLine ep) (sourceColumn ep)
      in Left $ ParseError posn
    Right v -> return v

removeComments :: String -> String
removeComments s =
  case elemIndex ';' s of
    Nothing -> s
    Just i -> take i s

removeAllComments :: String -> String
removeAllComments = unlines . map removeComments . lines


whitespace :: Parser ()
whitespace =
  void $ space <|> newline <|> tab

whitespaces :: Parser ()
whitespaces =
  void $ many whitespace

sp :: Parser a -> Parser a
sp x = whitespaces *> x <* whitespaces

parens :: Parser a -> Parser a
parens p =
  between (char '(') (char ')') p
  <|> between (char '[') (char ']') p


parseTree :: Parser Tree
parseTree =
  sp parseList
  <|> sp (do p <- getPosition
             con <- parseConstant
             return $ TreeVal con (SP (sourceLine p) (sourceColumn p)))
  <|> sp parseIdentifier
  <|> sp parseQuote

parseIdInitial :: Parser Char
parseIdInitial =
  letter <|>
  char '!' <|> char '$' <|> char '%' <|> char '&' <|> char '*'
  <|> char '/' <|> char ':' <|> char '<' <|> char '=' <|>
  char '>' <|> char '?' <|> char '~' <|> char '_' <|> char '^'

parseIdSubsequent :: Parser Char
parseIdSubsequent =
  parseIdInitial <|> digit <|> char '.' <|> char '-' <|> char '+'

parseSpecialIdentifier :: Parser Tree
parseSpecialIdentifier = do
  pos <- getPosition
  iden <- string "..." <|> string "+" <|> string "-"
  return $ TreeId iden (SP (sourceLine pos) (sourceColumn pos))

parseIdentifierStd :: Parser Tree
parseIdentifierStd = do
  pos <- getPosition
  initial <- parseIdInitial
  rest <- many parseIdSubsequent
  return (TreeId (initial:rest) (SP (sourceLine pos) (sourceColumn pos)))

parseIdentifier :: Parser Tree
parseIdentifier = try parseSpecialIdentifier <|> parseIdentifierStd

parseList :: Parser Tree
parseList = do
  p <- getPosition
  pts <- parens (many parseTree)
  return $ TreeSList pts (SP (sourceLine p) (sourceColumn p))

parseQuote :: Parser Tree
parseQuote = do
  p <- getPosition
  t <- string "'" >> parseTree
  let pos = SP (sourceLine p) (sourceColumn p)
  return $ TreeSList [TreeId "quote" pos, t] pos

parseConstant :: Parser Constant
parseConstant
   = parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> try parseInt

parseBool :: Parser Constant
parseBool
   = try (string "#t" >>= const (return . SBool $ True))
  <|> try (string "#f" >>= const (return . SBool $ False))

parseString :: Parser Constant
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ SStr val

parseInt :: Parser Constant
parseInt = do
  neg <- optionMaybe (char '-')
  case neg of
    Just _ ->
      SInt . (*(-1)) . read <$> many1 digit
    Nothing ->
      SInt . read <$> many1 digit

parseDouble :: Parser Constant
parseDouble =
  parseLeftDouble <|> parseRightDouble

parseLeftDouble :: Parser Constant
parseLeftDouble = do
  nums <- many1 digit
  decimalPoint <- string "."
  decimals <- flip mappend "0" <$> many digit
  return . SNum . read $ mappend nums $ mappend decimalPoint decimals

parseRightDouble :: Parser Constant
parseRightDouble = do
  nums <- mappend "0" <$> many digit
  decimalPoint <- string "."
  decimals <- many1 digit
  return . SNum . read $ mappend nums $ mappend decimalPoint decimals
