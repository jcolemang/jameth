
module Parse.ParseLambdaTest where

import Scheme.Types
import Scheme.Parse
import Parse.Helpers

import Test.HUnit

parseLambdas :: Test
parseLambdas = TestCase $ do
  let r1 = runParse "((lambda (y) (lambda (y) 2)) (- 2 0))"
  case r1 of
    Left err ->
      assertFailure $ "Did not parse: " ++ show err
    Right (Program [f]) ->
      assertBool "Is an application" (isApp f)
    _ ->
      assertFailure "Incorrect Parse"


tests :: [Test]
tests =
  [ parseLambdas
  ]
