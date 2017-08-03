
module ParseTest where

import JLTypes
import JLParse
import Test.HUnit


intTest :: Test
intTest = TestCase $
  case parseJL "123" of
    Left e  -> assertFailure . show $ e
    Right e -> assertEqual "Should be a constant" e (JLConstant $ JLInt 123)


basicDoubleTest :: Test
basicDoubleTest = TestCase $
  case parseJL "123.456" of
    Left e  -> assertFailure . show $ e
    Right e -> assertEqual "Should parse" e (JLConstant $ JLNum 123.456)


leftDoubleTest :: Test
leftDoubleTest = TestCase $
  case parseJL "123." of
    Left e  -> assertFailure . show $ e
    Right e -> assertEqual "Left heavy doubles should parse" e (JLConstant $ JLNum 123)


rightDoubleTest :: Test
rightDoubleTest = TestCase $
  case parseJL ".12345" of
    Left e  -> assertFailure . show $ e
    Right e -> assertEqual "Right heavy doubles should parse" e (JLConstant $ JLNum 0.12345)


basicStr :: Test
basicStr = TestCase $
  let r = "hello"
  in case parseJL "\"hello\"" of
    Left e -> assertFailure . show $ e
    Right e -> assertEqual "The string 123, not an int" e (JLConstant $ JLStr r)


numInString :: Test
numInString = TestCase $
  let r = "123"
  in case parseJL "\"123\"" of
    Left e -> assertFailure . show $ e
    Right e -> assertEqual "The string 123, not an int" e (JLConstant $ JLStr r)


trueBool :: Test
trueBool = TestCase $
  case parseJL "#t" of
    Left e -> assertFailure . show $ e
    Right e -> assertEqual "Equal to true" e (JLConstant $ JLBool True)


falseBool :: Test
falseBool = TestCase $
  case parseJL "#f" of
    Left e -> assertFailure . show $ e
    Right e -> assertEqual "Equal to false" e (JLConstant $ JLBool False)


parseValiableBasic :: Test
parseValiableBasic = TestCase $
  case parseJL "var" of
    Left e -> assertFailure . show $ e
    Right e -> assertEqual "Parsed variable" e (JLVariable "var")


tests :: [Test]
tests =
  [ TestLabel "Basic int" intTest
  , TestLabel "Basic double" basicDoubleTest
  , TestLabel "Left heavy double" leftDoubleTest
  , TestLabel "Right heavy double" rightDoubleTest
  , TestLabel "Basic string" basicStr
  , TestLabel "Number in a string" numInString
  , TestLabel "Parsing true" trueBool
  , TestLabel "Parsing false" falseBool
  ]
