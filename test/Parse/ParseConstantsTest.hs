
module Parse.ParseConstantsTest where

import Scheme.JLParse
import Scheme.JLTypes

import Test.HUnit


getConstant :: Either t JLProgram -> Maybe JLConstant
getConstant (Right (JLProgram [JLValue (JLConst x) _])) = Just x
getConstant _ = Nothing


testIntConstants :: Test
testIntConstants = TestCase $ do
  let r1 = getConstant (runJLParse "5")
  assertEqual "Basic Int Constant" r1 (Just $ JLInt 5)

  let r2 = getConstant (runJLParse "123")
  assertEqual "Multi-digit Int Constant" r2 (Just $ JLInt 123)

testNumConstants :: Test
testNumConstants = TestCase $ do
  let r1 = getConstant (runJLParse "1.0")
  assertEqual "Basic Num Constant" r1 (Just $ JLNum 1)

  let r2 = getConstant (runJLParse "1.5")
  assertEqual "Basic Num Constant" r2 (Just $ JLNum 1.5)

  let r3 = getConstant (runJLParse "1.")
  assertEqual "Right Num Constant" r3 (Just $ JLNum 1)

  let r4 = getConstant (runJLParse ".5")
  assertEqual "Left Num Constant" r4 (Just $ JLNum 0.5)

testBoolConstants :: Test
testBoolConstants = TestCase $ do
  let r1 = getConstant (runJLParse "#t")
  assertEqual "Basic #t" r1 (Just $ JLBool True)

  let r2 = getConstant (runJLParse "#f")
  assertEqual "Basic #f" r2 (Just $ JLBool False)

testStringConstants :: Test
testStringConstants = TestCase $ do
  let r1 = getConstant (runJLParse "\"Hello\"")
  assertEqual "Basic String" r1 (Just $ JLStr "Hello")

  let r2 = getConstant (runJLParse "\"Hello \n my name is coleman\"")
  assertEqual "Another String" r2 (Just $ JLStr "Hello \n my name is coleman")


tests :: [Test]
tests =
  [ testIntConstants
  , testNumConstants
  , testBoolConstants
  , testStringConstants
  ]
