
module Parse.ParseApplicationTest where

import Scheme.Types
import Scheme.Parse
import Parse.Helpers

import Test.HUnit
import Debug.Trace

isRawApp :: RawForm -> Bool
isRawApp (App _ _) = True
isRawApp _ = False

parseBasicApplication :: Test
parseBasicApplication = TestCase $ do
  let r1 = getApp =<< getFirstForm (runParseNoInit "(+)")
  assertBool "Result is application" $ maybe False isRawApp r1

  let r2 = getFirstFormA (runParseNoInit "(lambda (x) x)")
  assertBool "Result is lambda" $ maybe False isLambda r2

tests :: [Test]
tests =
  [ parseBasicApplication
  ]
