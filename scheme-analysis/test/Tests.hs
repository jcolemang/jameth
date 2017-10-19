
import Parse.ParseConstantsTest
import Parse.ParseQuoteTest
import Parse.ParseApplicationTest
import Parse.TokenizeTest
import Evaluation.EvaluationTest

import Test.HUnit

main :: IO ()
main = mapM_ runTestTT
       ( Parse.ParseConstantsTest.tests
       ++ Parse.ParseQuoteTest.tests
       ++ Parse.ParseApplicationTest.tests
       ++ Parse.TokenizeTest.tests
       ++ Evaluation.EvaluationTest.tests
       )
