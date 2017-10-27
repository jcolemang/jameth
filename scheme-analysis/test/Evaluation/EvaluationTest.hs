
module Evaluation.EvaluationTest where

import Scheme.Types
import Scheme.Parse
import Interpreter.Evaluate
import Interpreter.Types

import Test.HUnit

dummy :: Annotation
dummy =
  Ann PrimitiveSource (-1)

testProg :: String -> String -> Value -> Assertion
testProg s p v =
  case runParse p of
    Left err ->
      assertFailure $ show err
    Right prog -> do
      result <- execEval prog
      case result of
        Left err ->
          assertFailure $ show err
        Right val ->
          assertEqual s v val

basicApplicationTests :: Test
basicApplicationTests = TestCase $ do
  let prog1 = "(+)"
  let prog2 = "(+ 1 2 3)"
  let prog3 = "(+ 1 (* 2 4))"
  let prog4 = "(- 1 2)"
  let prog5 = "(- 1 2 3)"
  let prog6 = "(- 5 1)"
  let prog7 = "(- 9)"

  testProg "Add no args"             prog1 (VConst $ SInt 0)
  testProg "Add with args"           prog2 (VConst $ SInt 6)
  testProg "Two procs"               prog3 (VConst $ SInt 9)
  testProg "Subtraction"             prog4 (VConst $ SInt (-1))
  testProg "Multi Arg subtraction"   prog5 (VConst $ SInt (-4))
  testProg "No negative subtraction" prog6 (VConst $ SInt 4)
  testProg "Single arg subtraction"  prog7 (VConst $ SInt (-9))

lambdaTests :: Test
lambdaTests = TestCase $ do
  let prog1 = "((lambda () 5))"
  let prog2 = "((lambda (x) x) 100)"
  let prog3 = "((lambda (x) ((lambda (x) x) 5)) 10)"
  let prog4 = "((lambda (x) ((lambda (x) x) 5)) ((lambda (x) 100)))"
  let prog5 = "((lambda (lambda) (+ lambda 10)) 15 )"
  let prog6 = "((lambda (lambda) (lambda 10)) (lambda (lambda) (+ lambda 1 2 3)))"

  testProg "Basic lambda"      prog1 (VConst $ SInt 5)
  testProg "Basic lambda args" prog2 (VConst $ SInt 100)
  testProg "Nested lambdas"    prog3 (VConst $ SInt 5)
  testProg "More lambdas"      prog4 (VConst $ SInt 5)
  testProg "Bad naming"        prog5 (VConst $ SInt 25)
  testProg "Extra bad naming"  prog6 (VConst $ SInt 16)

tests :: [Test]
tests =
  [ basicApplicationTests
  , lambdaTests
  ]
