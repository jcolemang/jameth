
module JLParse where

import JLTypes
import Text.ParserCombinators.Parsec
import Control.Monad.Trans.Either
import Data.List

-- | Helpers

removeComments :: String -> String
removeComments s =
  case elemIndex ';' s of
    Nothing -> s
    Just i -> take i s


whitespace =
  space <|> newline <|> tab

whitespaces =
  many whitespace

whitespaces1 =
  whitespace >> whitespaces

-- | Public API

parseJL :: (Monad m) => String -> EitherT ParseError m JLProgram
parseJL code =
  let noComments = unlines $ map removeComments (lines code)
  in hoistEither $ parse (parseProgram <* eof) "jameth" noComments


-- | <program> --> <form>*

parseProgram :: Parser JLProgram
parseProgram =
  whitespaces *> (JLProgram <$> many parseForm) <* whitespaces


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
  _ <- char '(' >> whitespaces >> string "define" >> whitespaces
  iden <- parseIdentifier <* whitespaces
  body <- parseExpression
  _ <- whitespaces >> char ')'
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
  flip JLValue (SP pos) <$> try parseConstant
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
  return . JLVar iden $ SP pos


-- | <quote> --> (quote <datum>) | '<datum>

parseQuote :: Parser JLExpression
parseQuote =
  undefined


-- | <lambda> --> (lambda <formals> <body>)
--              | (case-lambda (<formals> <body>) ...)

parseLambda :: Parser JLExpression
parseLambda = do
  pos <- getPosition
  _ <- char '(' >> whitespaces >> string "lambda" >> whitespaces
  forms <- try parseFormals <* whitespaces
  body <- parseBody
  _ <- whitespaces >> char ')'
  return . JLLambda forms body $ SP pos


parseFormals :: Parser JLFormals
parseFormals =
  try parseListFormals
  <|> parseSymbolFormals
  <|> parseImproperFormals

parseListFormals :: Parser JLFormals
parseListFormals = do
  _ <- char '(' >> whitespaces
  forms <- sepEndBy parseIdentifier whitespaces
  _ <- whitespaces >> char ')'
  return $ JLFormals forms

parseSymbolFormals :: Parser JLFormals
parseSymbolFormals =
  JLSymbolFormal <$> parseIdentifier

parseImproperFormals :: Parser JLFormals
parseImproperFormals = do
  _ <- char '(' >> whitespaces
  ffirst <- parseIdentifier <* whitespaces
  frest <- many parseIdentifier
  _ <- whitespaces >> char '.' >> whitespaces
  flast <- parseIdentifier
  _ <- whitespaces >> char ')'
  return $ JLImproperFormals ffirst frest flast


parseBody :: Parser JLBody
parseBody = do
  defs <- sepEndBy (try parseDefinition) whitespaces
  fexp <- parseExpression <* whitespaces
  restExps <- sepEndBy parseExpression whitespaces
  return $ JLBody defs (fexp, restExps)


-- | <if> --> (if <expression> <expression> <expression>)
--          | (if <expression> <expression>)

parseIf :: Parser JLExpression
parseIf
   =  try parseTwoIf
  <|> parseOneIf

parseTwoIf :: Parser JLExpression
parseTwoIf = do
  pos <- getPosition
  _ <- char '(' >> whitespaces >> string "if" >> whitespaces
  cond <- parseExpression <* whitespaces
  ifthen <- parseExpression <* whitespaces
  ifelse <- parseExpression <* whitespaces
  _ <- char ')'
  return . JLTwoIf cond ifthen ifelse $ SP pos

parseOneIf :: Parser JLExpression
parseOneIf = do
  pos <- getPosition
  _ <- char '(' >> whitespaces
  _ <- string "if" >> whitespaces
  cond <- parseExpression <* whitespaces
  ifthen <- parseExpression <* whitespaces
  _ <- char ')'
  return . JLOneIf cond ifthen $ SP pos



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
  pos <- getPosition
  _ <- char '(' >> whitespaces
  f <- parseExpression <* whitespaces
  args <- sepEndBy parseExpression whitespaces
  _ <- char ')'
  return . JLApp f args $ SP pos
