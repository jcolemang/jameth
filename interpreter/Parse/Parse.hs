
module JLParse where


import JLTypes
import Text.ParserCombinators.Parsec


parseInt :: Parser JLLiteral
parseInt =
  JLInt . read <$> many1 digit

parseLeftDouble :: Parser JLLiteral
parseLeftDouble = do
  nums <- many1 digit
  decimalPoint <- string "."
  decimals <- flip mappend "0" <$> many digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals

parseRightDouble :: Parser JLLiteral
parseRightDouble = do
  nums <- mappend "0" <$> many digit
  decimalPoint <- string "."
  decimals <- many1 digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals

parseDouble :: Parser JLLiteral
parseDouble =
  parseLeftDouble <|> parseRightDouble

parseString :: Parser JLLiteral
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ JLStr val

parseBool :: Parser JLLiteral
parseBool
   =  try (string "#t" >>= const (return . JLBool $ True))
  <|> (string "#f" >>= const (return . JLBool $ False))

parseConstant :: Parser JLLiteral
parseConstant
   =  parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> parseInt


parseVariable :: Parser JLExpression
parseVariable =
  JLVariable <$> many1 letter


parseExpression :: Parser JLExpression
parseExpression
   =  (JLConstant <$> parseConstant)
  <|> parseVariable


parseJL :: String -> Either ParseError JLExpression
parseJL =
  flip parse ""
   $  do val <- parseExpression
         eof
         return val
