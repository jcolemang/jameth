
module Evaluation.EvaluationTest where

import Scheme.Types
import Scheme.Parse
import Interpreter.Evaluate

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

  testProg "Add no args"       prog1 (Const $ SInt 0)
  testProg "Add with args"     prog2 (Const $ SInt 6)
  testProg "Two procs"         prog3 (Const $ SInt 9)

lambdaTests :: Test
lambdaTests = TestCase $ do
  let prog1 = "((lambda () 5))"
  let prog2 = "((lambda (x) x) 100)"
  let prog3 = "((lambda (x) ((lambda (x) x) 5)) 10)"
  let prog4 = "((lambda (x) ((lambda (x) x) 5)) ((lambda (x) 100)))"
  let prog5 = "((lambda (lambda) (+ lambda 10)) 15 )"
  let prog6 = "((lambda (lambda) (lambda 10)) (lambda (lambda) (+ lambda 1 2 3)))"

  testProg "Basic lambda"      prog1 (Const $ SInt 5)
  testProg "Basic lambda args" prog2 (Const $ SInt 100)
  testProg "Nested lambdas"    prog3 (Const $ SInt 5)
  testProg "More lambdas"      prog4 (Const $ SInt 5)
  testProg "Bad naming"        prog5 (Const $ SInt 25)
  testProg "Extra bad naming"  prog6 (Const $ SInt 16)

tests :: [Test]
tests =
  [ basicApplicationTests
  , lambdaTests
  ]
