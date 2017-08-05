
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
  <|> JLFormDef <$> parseDefinition

-- | <definition> --> <variable def>
--                  | <syntax definition>
--                  | (begin <definition>*)
--                  | <module form>
--                  | <import form>
--                  | <meta definition>
--                  | <alias form>
--                  | (let-syntax (<syntax binding>*) <definition>*)
--                  | (letrec-syntax (<syntax binding>*) <definition>*)
--                  | <derived definition>

parseDefinition :: Parser JLDefinition
parseDefinition =
  JLVarDef <$> parseVariableDef

-- | <variable def> --> (define <variable> <expression>)
--                    | (define <variable>)
--                    | (define (<variable> <variable>*) <body>)
--                    | (define (<variable> <variable>* . <variable>) <body>)

parseVariableDef :: Parser JLVariableDefinition
parseVariableDef = do
  _ <- char '(' >> spaces >> string "define" >> spaces
  iden <- parseIdentifier <* spaces
  body <- parseExpression
  _ <- spaces >> char ')'
  return $ JLDefine iden body


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
  <|> try parseLambda
  <|> try parseApplication


-- | <constant> --> <boolean> | <number> | <character> | <string> | <special>

parseConstant :: Parser JLValue
parseConstant
   = parseBool
  <|> parseString
  <|> try parseDouble  -- same start as parseInt, should not consume
  <|> parseInt


-- | <boolean> --> #t | #f

parseBool :: Parser JLValue
parseBool
   = try (string "#t" >>= const (return . JLBool $ True))
  <|> try (string "#f" >>= const (return . JLBool $ False))


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
parseVariable = do
  pos <- getPosition
  iden <- parseIdentifier
  return $ JLVar iden pos


-- | <quote> --> (quote <datum>) | '<datum>

parseQuote :: Parser JLExpression
parseQuote =
  undefined


-- | <lambda> --> (lambda <formals> <body>)
--              | (case-lambda (<formals> <body>) ...)

parseLambda :: Parser JLExpression
parseLambda = do
  _ <- char '(' >> spaces >> string "lambda" >> spaces
  pos <- getPosition
  forms <- try parseFormals <* spaces
  body <- parseBody
  _ <- spaces >> char ')'
  return $ JLLambda forms body pos


parseFormals :: Parser JLFormals
parseFormals =
  try parseListFormals
  <|> parseSymbolFormals
  <|> parseImproperFormals

parseListFormals :: Parser JLFormals
parseListFormals = do
  _ <- char '(' >> spaces
  forms <- sepEndBy parseIdentifier spaces
  _ <- spaces >> char ')'
  return $ JLFormals forms

parseSymbolFormals :: Parser JLFormals
parseSymbolFormals =
  JLSymbolFormal <$> parseIdentifier

parseImproperFormals :: Parser JLFormals
parseImproperFormals = do
  _ <- char '(' >> spaces
  ffirst <- parseIdentifier <* spaces
  frest <- many parseIdentifier
  _ <- spaces >> char '.' >> spaces
  flast <- parseIdentifier
  _ <- spaces >> char ')'
  return $ JLImproperFormals (ffirst, frest, flast)


parseBody :: Parser JLBody
parseBody = do
  defs <- sepEndBy (try parseDefinition) spaces
  fexp <- parseExpression <* spaces
  restExps <- sepEndBy parseExpression spaces
  return $ JLBody defs (fexp, restExps)


-- | <if> --> (if <expression> <expression> <expression>)
--          | (if <expression> <expression>)

parseIf :: Parser JLExpression
parseIf
   =  try parseTwoIf
  <|> parseOneIf

parseTwoIf :: Parser JLExpression
parseTwoIf = do
  _ <- char '(' >> spaces >> string "if" >> spaces
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

parseIdentifier :: Parser String
parseIdentifier = do
  first <- validIdFirstSymbols
  rest <- many validIdSymbols
  return (first:rest)


-- | <application> --> (<expression> <expression>*)

parseApplication :: Parser JLExpression
parseApplication = do
  _ <- char '(' >> spaces
  pos <- getPosition
  f <- parseExpression <* spaces
  args <- sepEndBy parseExpression spaces
  _ <- char ')'
  return $ JLApp f args pos
