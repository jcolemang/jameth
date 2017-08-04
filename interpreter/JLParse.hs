
module JLParse where

import JLTypes
import Text.ParserCombinators.Parsec
import Control.Monad.Trans.Either


-- | Public API

parseJL :: (Monad m) => String -> EitherT ParseError m (JLProgram m)
parseJL =
  hoistEither . parse (parseProgram <* eof) ""


-- | <program> --> <form>*

parseProgram :: Parser (JLProgram m)
parseProgram =
  spaces *> (JLProgram <$> many parseForm) <* spaces


-- | <form> --> <definition> | <expression>

parseForm :: Parser (JLForm m)
parseForm =
  JLFormExp <$> parseExpression


-- | <expression> --> <constant>
--                  | <variable>
--                  | <quote>
--                  | <lambda>
--                  | <if>
--                  | (set! <variable> <expression>)
--                  | <application>
--                  | (let-syntax (<syntax binding>*) <expression>+)
--                  | (letrec-syntax (<syntax binding>*) <expression>+)
--                  | <derived expression>

parseExpression :: Parser (JLExpression m)
parseExpression = do
  pos <- getPosition
  flip JLValue pos <$> parseConstant
  <|> try parseVariable
  <|> try parseIf -- same start as application
  <|> try parseApplication


-- | <constant> --> <boolean> | <number> | <character> | <string> | <special>

parseConstant :: Parser (JLValue m)
parseConstant
   =  parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> parseInt


-- | <boolean> --> #t | #f

parseBool :: Parser (JLValue m)
parseBool
   =  try (string "#t" >>= const (return . JLBool $ True))
  <|> (string "#f" >>= const (return . JLBool $ False))


-- | <string> --> "<string-character>*"

parseString :: Parser (JLValue m)
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ JLStr val


-- | <number> --> <int> | <double>

-- | <int> --> <digit>+

parseInt :: Parser (JLValue m)
parseInt =
  JLInt . read <$> many1 digit


-- | <double> --> <digit>*.<digit>+ | <digit>+.<digit>*

parseDouble :: Parser (JLValue m)
parseDouble =
  parseLeftDouble <|> parseRightDouble

parseLeftDouble :: Parser (JLValue m)
parseLeftDouble = do
  nums <- many1 digit
  decimalPoint <- string "."
  decimals <- flip mappend "0" <$> many digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals

parseRightDouble :: Parser (JLValue m)
parseRightDouble = do
  nums <- mappend "0" <$> many digit
  decimalPoint <- string "."
  decimals <- many1 digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals


-- | <variable> --> <identifier>

parseVariable :: Parser (JLExpression m)
parseVariable = parseIdentifier


-- | <quote> --> (quote <datum>) | '<datum>

parseQuote :: Parser (JLExpression m)
parseQuote =
  undefined


-- | <lambda> --> (lambda <formals> <body>) | (case-lambda (<formals> <body>) ...)

parseLambda :: Parser (JLExpression m)
parseLambda = undefined


-- | <if> --> (if <expression> <expression> <expression>) | (if <expression> <expression>)

parseIf :: Parser (JLExpression m)
parseIf
   =  try parseTwoIf
  <|> parseOneIf

parseTwoIf :: Parser (JLExpression m)
parseTwoIf = do
  _ <- char '(' >> spaces
  _ <- string "if" >> spaces
  pos <- getPosition
  cond <- parseExpression <* spaces
  ifthen <- parseExpression <* spaces
  ifelse <- parseExpression <* spaces
  _ <- char ')'
  return $ JLTwoIf cond ifthen ifelse pos

parseOneIf :: Parser (JLExpression m)
parseOneIf = do
  _ <- char '(' >> spaces
  _ <- string "if" >> spaces
  pos <- getPosition
  cond <- parseExpression <* spaces
  ifthen <- parseExpression <* spaces
  _ <- char ')'
  return $ JLOneIf cond ifthen pos



-- | <identifier> --> <letter>*

parseIdentifier :: Parser (JLExpression m)
parseIdentifier =
  getPosition >>= \pos -> many1 (letter <|> char '+') >>= \x -> return $ JLVar x pos


-- | <application> --> (<expression> <expression>*)

parseApplication :: Parser (JLExpression m)
parseApplication = do
  _ <- char '(' >> spaces
  pos <- getPosition
  f <- parseExpression <* spaces
  args <- sepEndBy parseExpression spaces
  _ <- char ')'
  return $ JLApp f args pos
