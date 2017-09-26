
module Scheme.JLTokenize (readJL,
                          JLTokenizeError,
                         ) where

import Scheme.JLTypes

import Text.ParserCombinators.Parsec
import Data.List
import Control.Monad.Trans.Except
import Control.Monad


readJL :: String -> Either JLParseError [JLTree]
readJL =
  tokenizeJL . removeAllComments

tokenizeJL :: String -> Either JLParseError [JLTree]
tokenizeJL s =
  case parse (whitespaces *> many parseTree <* whitespaces <* eof) "jameth" s of
    Left p -> Left . JLParseError $ (SP $ errorPos p)
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
parens = between (char '(') (char ')')


parseTree :: Parser JLTree
parseTree =
  sp parseList
  <|> sp (do p <- getPosition
             con <- parseConstant
             return $ JLVal con (SP p))
  <|> sp parseIdentifier

parseIdInitial :: Parser Char
parseIdInitial =
  letter <|>
  char '!' <|> char '$' <|> char '%' <|> char '&' <|> char '*'
  <|> char '/' <|> char ':' <|> char '<' <|> char '=' <|>
  char '>' <|> char '?' <|> char '~' <|> char '_' <|> char '^'

parseIdSubsequent :: Parser Char
parseIdSubsequent =
  parseIdInitial <|> digit <|> char '.' <|> char '-' <|> char '+'

parseSpecialIdentifier :: Parser JLTree
parseSpecialIdentifier = do
  pos <- getPosition
  iden <- string "..." <|> string "+" <|> string "-"
  return $ JLId iden (SP pos)

parseIdentifierStd :: Parser JLTree
parseIdentifierStd = do
  pos <- getPosition
  initial <- parseIdInitial
  rest <- many parseIdSubsequent
  return (JLId (initial:rest) (SP pos))

parseIdentifier :: Parser JLTree
parseIdentifier = try parseSpecialIdentifier <|> parseIdentifierStd

parseList :: Parser JLTree
parseList = do
  p <- getPosition
  pts <- parens (many parseTree)
  return $ JLSList pts (SP p)

parseConstant :: Parser JLConstant
parseConstant
   = parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> parseInt

parseBool :: Parser JLConstant
parseBool
   = try (string "#t" >>= const (return . JLBool $ True))
  <|> try (string "#f" >>= const (return . JLBool $ False))

parseString :: Parser JLConstant
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ JLStr val

parseInt :: Parser JLConstant
parseInt =
  JLInt . read <$> many1 digit

parseDouble :: Parser JLConstant
parseDouble =
  parseLeftDouble <|> parseRightDouble

parseLeftDouble :: Parser JLConstant
parseLeftDouble = do
  nums <- many1 digit
  decimalPoint <- string "."
  decimals <- flip mappend "0" <$> many digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals

parseRightDouble :: Parser JLConstant
parseRightDouble = do
  nums <- mappend "0" <$> many digit
  decimalPoint <- string "."
  decimals <- many1 digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals
