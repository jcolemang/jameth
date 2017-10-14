
module Scheme.JLTokenize (readJL,
                         ) where

import Scheme.JLParsingTypes
import Scheme.JLTypes

import Text.ParserCombinators.Parsec
import Data.List
import Control.Monad

readJL :: String -> Either JLParseError [JLTree]
readJL =
  tokenizeJL . removeAllComments

tokenizeJL :: String -> Either JLParseError [JLTree]
tokenizeJL s =
  case parse (whitespaces *> many parseTree <* whitespaces <* eof) "jameth" s of
    Left p ->
      let ep = errorPos p
          pos = SP (sourceLine ep) (sourceColumn ep)
      in Left $ JLParseError pos
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


parseTree :: Parser JLTree
parseTree =
  sp parseList
  <|> sp (do p <- getPosition
             con <- parseConstant
             return $ JLVal con (SP (sourceLine p) (sourceColumn p)))
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

parseSpecialIdentifier :: Parser JLTree
parseSpecialIdentifier = do
  pos <- getPosition
  iden <- string "..." <|> string "+" <|> string "-"
  return $ JLId iden (SP (sourceLine pos) (sourceColumn pos))

parseIdentifierStd :: Parser JLTree
parseIdentifierStd = do
  pos <- getPosition
  initial <- parseIdInitial
  rest <- many parseIdSubsequent
  return (JLId (initial:rest) (SP (sourceLine pos) (sourceColumn pos)))

parseIdentifier :: Parser JLTree
parseIdentifier = try parseSpecialIdentifier <|> parseIdentifierStd

parseList :: Parser JLTree
parseList = do
  p <- getPosition
  pts <- parens (many parseTree)
  return $ JLSList pts (SP (sourceLine p) (sourceColumn p))

parseQuote :: Parser JLTree
parseQuote = do
  p <- getPosition
  t <- string "'" >> parseTree
  let pos = SP (sourceLine p) (sourceColumn p)
  return $ JLSList [JLId "quote" pos, t] pos

parseConstant :: Parser Constant
parseConstant
   = parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> parseInt

parseBool :: Parser Constant
parseBool
   = try (string "#t" >>= const (return . JLBool $ True))
  <|> try (string "#f" >>= const (return . JLBool $ False))

parseString :: Parser Constant
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ JLStr val

parseInt :: Parser Constant
parseInt =
  JLInt . read <$> many1 digit

parseDouble :: Parser Constant
parseDouble =
  parseLeftDouble <|> parseRightDouble

parseLeftDouble :: Parser Constant
parseLeftDouble = do
  nums <- many1 digit
  decimalPoint <- string "."
  decimals <- flip mappend "0" <$> many digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals

parseRightDouble :: Parser Constant
parseRightDouble = do
  nums <- mappend "0" <$> many digit
  decimalPoint <- string "."
  decimals <- many1 digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals
