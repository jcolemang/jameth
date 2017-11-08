
module Parse.ParseApplicationTest where

import Scheme.Types
import Scheme.Parse
import Parse.Helpers

import Test.HUnit

isRawApp :: RawForm a -> Bool
isRawApp (App _ _) = True
isRawApp _ = False

parseBasicApplication :: Test
parseBasicApplication = TestCase $ do
  let r1 = getApp =<< getFirstForm (runParse "(+)")
  assertBool "Result is application" $ maybe False isRawApp r1

  let r2 = getFirstFormA (runParse "(lambda (x) x)")
  assertBool "Result is lambda" $ maybe False isLambda r2

  let r4 = getSecondForm (runParse ("(lambda (lambda) lambda)" ++
                                          "(lambda (lambda) lambda)"))
  case r4 of
    Just (Lambda _ [body]) ->
      assertBool "Body is a variable" $ isVar body
    Just _ ->
      assertFailure "Environment escaping to second expression"
    _ ->
      assertFailure "Lambda shadowing error"

tests :: [Test]
tests =
  [ parseBasicApplication
  ]
