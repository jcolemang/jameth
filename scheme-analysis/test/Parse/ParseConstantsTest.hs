
module Parse.ParseConstantsTest where

import Scheme.Parse
import Scheme.Types

import Test.HUnit


getConstant :: Either t (Program Annotation) -> Maybe Constant
getConstant (Right (Program [A _ (Const x)])) = Just x
getConstant _ = Nothing


testIntConstants :: Test
testIntConstants = TestCase $ do
  let r1 = getConstant (runParse "5")
  assertEqual "Basic Int Constant" (Just $ SInt 5) r1

  let r2 = getConstant (runParse "123")
  assertEqual "Multi-digit Int Constant" (Just $ SInt 123) r2

testNumConstants :: Test
testNumConstants = TestCase $ do
  let r1 = getConstant (runParse "1.0")
  assertEqual "Basic Num Constant" (Just $ SNum 1) r1

  let r2 = getConstant (runParse "1.5")
  assertEqual "Basic Num Constant" (Just $ SNum 1.5) r2

  let r3 = getConstant (runParse "1.")
  assertEqual "Right Num Constant" (Just $ SNum 1) r3

  let r4 = getConstant (runParse ".5")
  assertEqual "Left Num Constant" (Just $ SNum 0.5) r4

testBoolConstants :: Test
testBoolConstants = TestCase $ do
  let r1 = getConstant (runParse "#t")
  assertEqual "Basic #t" (Just $ SBool True) r1

  let r2 = getConstant (runParse "#f")
  assertEqual "Basic #f" (Just $ SBool False) r2

testStringConstants :: Test
testStringConstants = TestCase $ do
  let r1 = getConstant (runParse "\"Hello\"")
  assertEqual "Basic String" (Just $ SStr "Hello") r1

  let r2 = getConstant (runParse "\"Hello \n my name is coleman\"")
  assertEqual "Another String" (Just $ SStr "Hello \n my name is coleman") r2


tests :: [Test]
tests =
  [ testIntConstants
  , testNumConstants
  , testBoolConstants
  , testStringConstants
  ]
