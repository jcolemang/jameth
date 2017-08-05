
module JLParse where

import JLTypes
import Text.ParserCombinators.Parsec
import Control.Monad.Trans.Either


-- | Public API

parseJL :: (Monad m) => String -> EitherT ParseError m JLProgram
parseJL =
  hoistEither . parse (parseProgram <* eof) ""


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
--                  | <quote>
--                  | <lambda>
--                  | <if>
--                  | (set! <variable> <expression>)
--                  | <application>
--                  | (let-syntax (<syntax binding>*) <expression>+)
--                  | (letrec-syntax (<syntax binding>*) <expression>+)
--                  | <derived expression>

parseExpression :: Parser JLExpression
parseExpression = do
  pos <- getPosition
  flip JLValue pos <$> try parseConstant
  <|> try parseVariable
  <|> try parseIf -- same start as application
  <|> try parseApplication


-- | <constant> --> <boolean> | <number> | <character> | <string> | <special>

parseConstant :: Parser JLValue
parseConstant
   =  parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> parseInt


-- | <boolean> --> #t | #f

parseBool :: Parser JLValue
parseBool
   =  try (string "#t" >>= const (return . JLBool $ True))
  <|> (string "#f" >>= const (return . JLBool $ False))


-- | <string> --> "<string-character>*"

parseString :: Parser JLValue
parseString = do
  _ <- char '"'
  val <- many $ noneOf "\""
  _ <- char '"'
  return $ JLStr val


-- | <number> --> <int> | <double>

-- | <int> --> <digit>+

parseInt :: Parser JLValue
parseInt =
  JLInt . read <$> many1 digit


-- | <double> --> <digit>*.<digit>+ | <digit>+.<digit>*

parseDouble :: Parser JLValue
parseDouble =
  parseLeftDouble <|> parseRightDouble

parseLeftDouble :: Parser JLValue
parseLeftDouble = do
  nums <- many1 digit
  decimalPoint <- string "."
  decimals <- flip mappend "0" <$> many digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals

parseRightDouble :: Parser JLValue
parseRightDouble = do
  nums <- mappend "0" <$> many digit
  decimalPoint <- string "."
  decimals <- many1 digit
  return . JLNum . read $ mappend nums $ mappend decimalPoint decimals


-- | <variable> --> <identifier>

parseVariable :: Parser JLExpression
parseVariable = parseIdentifier


-- | <quote> --> (quote <datum>) | '<datum>

parseQuote :: Parser JLExpression
parseQuote =
  undefined


-- | <lambda> --> (lambda <formals> <body>) | (case-lambda (<formals> <body>) ...)

parseLambda :: Parser JLExpression
parseLambda = undefined


-- | <if> --> (if <expression> <expression> <expression>) | (if <expression> <expression>)

parseIf :: Parser JLExpression
parseIf
   =  try parseTwoIf
  <|> parseOneIf

parseTwoIf :: Parser JLExpression
parseTwoIf = do
  _ <- char '(' >> spaces
  _ <- string "if" >> spaces
  pos <- getPosition
  cond <- parseExpression <* spaces
  ifthen <- parseExpression <* spaces
  ifelse <- parseExpression <* spaces
  _ <- char ')'
  return $ JLTwoIf cond ifthen ifelse pos

parseOneIf :: Parser JLExpression
parseOneIf = do
  _ <- char '(' >> spaces
  _ <- string "if" >> spaces
  pos <- getPosition
  cond <- parseExpression <* spaces
  ifthen <- parseExpression <* spaces
  _ <- char ')'
  return $ JLOneIf cond ifthen pos



-- | <identifier> --> <letter>*

validIdFirstSymbols =
  letter <|> char '.' <|>  char '+' <|>  char '-' <|>  char '!' <|>  char '$'
  <|>  char '%' <|>  char '&' <|>  char '*' <|>  char '/' <|>  char ':'
  <|>  char '<' <|>  char '=' <|>  char '>' <|>  char '?' <|>  char '~'
  <|>  char '_' <|>  char '^' <|>  char '@'

validIdSymbols =
  char '#' <|> validIdFirstSymbols

parseIdentifier :: Parser JLExpression
parseIdentifier = do
  pos <- getPosition
  first <- validIdFirstSymbols
  rest <- many validIdSymbols
  return $ JLVar (first:rest) pos


-- | <application> --> (<expression> <expression>*)

parseApplication :: Parser JLExpression
parseApplication = do
  _ <- char '(' >> spaces
  pos <- getPosition
  f <- parseExpression <* spaces
  args <- sepEndBy parseExpression spaces
  _ <- char ')'
  return $ JLApp f args pos
