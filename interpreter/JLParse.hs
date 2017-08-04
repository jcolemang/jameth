
module JLParse where

import JLTypes
import Text.ParserCombinators.Parsec


-- | Public API

parseJL :: String -> Either ParseError JLProgram
parseJL =
  flip parse ""
   $  do val <- parseProgram
         eof
         return val


-- | <program> --> <form>*

parseProgram :: Parser JLProgram
parseProgram =
  spaces *> (JLProgram <$> many parseForm) <* spaces


-- | <form> --> <definition> | <expression>

parseForm :: Parser JLForm
parseForm =
  JLFormExp <$> parseExpression


-- | <expression> --> <constant>
--                  | <variable>
--                  | (quote <datum>) | '<datum>
--                  | (lambda <formals> <body>)
--                  | (case-lambda (<formals> <body>) ...)
--                  | (if <expression> <expression> <expression>) | (if <expression> <expression>)
--                  | (set! <variable> <expression>)
--                  | <application>
--                  | (let-syntax (<syntax binding>*) <expression>+)
--                  | (letrec-syntax (<syntax binding>*) <expression>+)
--                  | <derived expression>

parseExpression :: Parser JLExpression
parseExpression
   =  (JLConst <$> parseConstant)
  <|> parseVariable
  <|> parseApplication


-- | <constant> --> <boolean> | <number> | <character> | <string> | <special>

parseConstant :: Parser JLLiteral
parseConstant
   =  parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> parseInt


-- | <boolean> --> #t | #f

parseBool :: Parser JLLiteral
parseBool
   =  try (string "#t" >>= const (return . JLBool $ True))
  <|> (string "#f" >>= const (return . JLBool $ False))


-- | <string> --> "<string-character>*"

parseString :: Parser JLLiteral
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ JLStr val


-- | <number> --> <int> | <double>

-- | <int> --> <digit>+

parseInt :: Parser JLLiteral
parseInt =
  JLInt . read <$> many1 digit


-- | <double> --> <digit>*.<digit>+ | <digit>+.<digit>*

parseDouble :: Parser JLLiteral
parseDouble =
  parseLeftDouble <|> parseRightDouble

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


-- | <variable> --> <identifier>

parseVariable :: Parser JLExpression
parseVariable = parseIdentifier


-- | <identifier> --> <letter>*

parseIdentifier :: Parser JLExpression
parseIdentifier =
  JLVar <$> many1 letter


-- | <application> --> (<expression> <expression>*)

parseApplication :: Parser JLExpression
parseApplication = do
  _ <- char '(' >> spaces
  f <- parseExpression <* spaces
  args <- sepEndBy parseExpression spaces
  _ <- char ')'
  return $ JLApp f args
