
module JLTokenize (tokenizeJL, readJL,
                   removeAllComments,
                   JLTokenizeError, JLTree)
where


import Text.ParserCombinators.Parsec
import Data.List
import Control.Monad.Trans.Except
import Control.Monad


data JLTree
  = JLVal JLConst
  | JLId    String
  | JLSList [JLTree]
  deriving (Show)

data JLConst
  = JLStr  String
  | JLBool Bool
  | JLInt  Integer
  | JLNum  Double
  | JLVoid
  deriving (Show, Eq)


type JLTokenizeError = ParseError


readJL :: (Monad m) => String -> ExceptT JLTokenizeError m [JLTree]
readJL =
  ExceptT . return . tokenizeJL . removeAllComments

tokenizeJL :: String -> Either JLTokenizeError [JLTree]
tokenizeJL = parse (whitespaces *> many parseTree <* whitespaces <* eof) "jameth"

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

whitespaces1 :: Parser ()
whitespaces1 =
  whitespace >> whitespaces

sp :: Parser a -> Parser a
sp x = whitespaces *> try x <* whitespaces


parseTree :: Parser JLTree
parseTree =
  sp parseList
  <|> sp (JLVal <$> parseConstant)
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
parseSpecialIdentifier =
  JLId <$> (string "..." <|> string "+" <|> string "-")

parseIdentifierStd :: Parser JLTree
parseIdentifierStd = do
  initial <- parseIdInitial
  rest <- many parseIdSubsequent
  return . JLId $ (initial:rest)

parseIdentifier :: Parser JLTree
parseIdentifier = try parseSpecialIdentifier <|> parseIdentifierStd

parseList :: Parser JLTree
parseList = do
  char '(' >> whitespaces
  xs <- many parseTree
  whitespaces <* char ')'
  return (JLSList xs)

parseConstant :: Parser JLConst
parseConstant
   = parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> try parseInt

parseBool :: Parser JLConst
parseBool
   = try (string "#t" >>= const (return . JLBool $ True))
  <|> try (string "#f" >>= const (return . JLBool $ False))

parseString :: Parser JLConst
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ JLStr val

parseInt :: Parser JLConst
parseInt =
  JLInt . read <$> many1 digit

parseDouble :: Parser JLConst
parseDouble =
  parseLeftDouble <|> parseRightDouble

parseLeftDouble :: Parser JLConst
parseLeftDouble = do
  nums <- many1 digit
  decimalPoint <- string "."
  decimals <- flip mappend "0" <$> many digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals

parseRightDouble :: Parser JLConst
parseRightDouble = do
  nums <- mappend "0" <$> many digit
  decimalPoint <- string "."
  decimals <- many1 digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals
