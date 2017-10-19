
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

evalIntegrationTests :: Test
evalIntegrationTests = TestCase $ do
  let prog1 = "(+)"
  let prog2 = "(+ 1 2 3)"
  let prog3 = "(+ 1 (* 2 4))"
  let prog4 = "((lambda () 5))"
  let prog5 = "((lambda (x) x) 100)"
  let prog6 = "((lambda (x) ((lambda (x) x) 5)) 10)"
  let prog7 = "((lambda (x) ((lambda (x) x) 5)) ((lambda (x) 100)))"
  let prog8 = "((lambda (lambda) lambda) (+ 1 2 3))"

  testProg "Add no args"       prog1 (Const $ SInt 0)
  testProg "Add with args"     prog2 (Const $ SInt 6)
  testProg "Two procs"         prog3 (Const $ SInt 9)
  testProg "Basic lambda"      prog4 (Const $ SInt 5)
  testProg "Basic lambda args" prog5 (Const $ SInt 100)
  testProg "Nested lambdas"    prog6 (Const $ SInt 5)
  testProg "More lambdas"      prog7 (Const $ SInt 5)
  testProg "Bad naming"        prog8 (Const $ SInt 6)

tests :: [Test]
tests =
  [ evalIntegrationTests
  ]
